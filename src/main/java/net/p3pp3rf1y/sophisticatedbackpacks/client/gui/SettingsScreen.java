package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.datafixers.util.Pair;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackSettingsTabControl;

import javax.annotation.Nullable;

public class SettingsScreen extends ContainerScreen<SettingsContainer> {
	private BackpackSettingsTabControl settingsTabControl;

	public SettingsScreen(SettingsContainer screenContainer, PlayerInventory inv, ITextComponent titleIn) {
		super(screenContainer, inv, titleIn);
		ySize = 114 + getContainer().getNumberOfRows() * 18;
		xSize = getContainer().getBackpackBackgroundProperties().getSlotsOnLine() * 18 + 14;
		playerInventoryTitleY = ySize - 94;
		playerInventoryTitleX = 8 + getContainer().getBackpackBackgroundProperties().getPlayerInventoryXOffset();
	}

	@Override
	protected void init() {
		super.init();

		settingsTabControl = new BackpackSettingsTabControl(this, new Position(guiLeft + xSize, guiTop + 4));
		children.add(settingsTabControl);
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(MatrixStack matrixStack, float partialTicks, int x, int y) {
		BackpackBackgroundProperties backpackBackgroundProperties = getContainer().getBackpackBackgroundProperties();
		BackpackGuiHelper.renderBackpackBackground(new Position((width - xSize) / 2, (height - ySize) / 2), matrixStack, getContainer().getBackpackInventorySlots().size(), backpackBackgroundProperties.getSlotsOnLine(), backpackBackgroundProperties.getTextureName(), xSize, minecraft);
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		container.detectSettingsChangeAndReload();
		renderBackground(matrixStack);
		settingsTabControl.render(matrixStack, mouseX, mouseY, partialTicks);
		matrixStack.translate(0, 0, 200);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
		settingsTabControl.afterScreenRender(matrixStack, mouseX, mouseY, partialTicks);
		renderHoveredTooltip(matrixStack, mouseX, mouseY);
	}

	@Override
	protected void drawGuiContainerForegroundLayer(MatrixStack matrixStack, int mouseX, int mouseY) {
		super.drawGuiContainerForegroundLayer(matrixStack, mouseX, mouseY);
		for (int slotId = 0; slotId < container.ghostSlots.size(); ++slotId) {
			Slot slot = container.ghostSlots.get(slotId);
			moveItems(matrixStack, slot);

			settingsTabControl.renderSlotOverlays(matrixStack, slot, this::renderSlotOverlay);

			if (isSlotSelected(slot, mouseX, mouseY) && slot.isEnabled()) {
				hoveredSlot = slot;
				renderSlotOverlay(matrixStack, slot, getSlotColor(slotId));
			}
		}
	}

	@Override
	protected void moveItems(MatrixStack matrixStack, Slot slot) {
		int i = slot.xPos;
		int j = slot.yPos;
		ItemStack itemstack = slot.getStack();
		boolean flag1 = slot == clickedSlot && !draggedStack.isEmpty() && !isRightMouseClick;

		setBlitOffset(100);
		itemRenderer.zLevel = 100.0F;
		if (itemstack.isEmpty() && slot.isEnabled()) {
			Pair<ResourceLocation, ResourceLocation> pair = slot.getBackground();
			if (pair != null) {
				TextureAtlasSprite textureatlassprite = minecraft.getAtlasSpriteGetter(pair.getFirst()).apply(pair.getSecond());
				minecraft.getTextureManager().bindTexture(textureatlassprite.getAtlasTexture().getTextureLocation());
				blit(matrixStack, i, j, getBlitOffset(), 16, 16, textureatlassprite);
				flag1 = true;
			}
		}

		if (!flag1) {
			RenderSystem.enableDepthTest();
			itemRenderer.renderItemAndEffectIntoGUI(minecraft.player, itemstack, i, j);
		}

		itemRenderer.zLevel = 0.0F;
		setBlitOffset(0);
	}

	@SuppressWarnings("java:S2589") // slot can actually be null despite being marked non null
	@Override
	protected void handleMouseClick(Slot slot, int slotId, int mouseButton, ClickType type) {
		//noinspection ConstantConditions
		if (slot != null) {
			settingsTabControl.handleSlotClick(slot, mouseButton);
		}
	}

	@Override
	public boolean mouseDragged(double mouseX, double mouseY, int button, double dragX, double dragY) {
		Slot slot = getSelectedSlot(mouseX, mouseY);
		if (slot != null) {
			settingsTabControl.handleSlotClick(slot, button);
		}
		return true;
	}

	@Nullable
	@Override
	protected Slot getSelectedSlot(double mouseX, double mouseY) {
		for (int i = 0; i < container.ghostSlots.size(); ++i) {
			Slot slot = container.ghostSlots.get(i);
			if (isSlotSelected(slot, mouseX, mouseY) && slot.isEnabled()) {
				return slot;
			}
		}

		return null;
	}

	@Override
	protected boolean hasClickedOutside(double mouseX, double mouseY, int guiLeftIn, int guiTopIn, int mouseButton) {
		return super.hasClickedOutside(mouseX, mouseY, guiLeftIn, guiTopIn, mouseButton) && hasClickedOutsideOfSettings(mouseX, mouseY);
	}

	private boolean hasClickedOutsideOfSettings(double mouseX, double mouseY) {
		return settingsTabControl.getTabRectangles().stream().noneMatch(r -> r.contains((int) mouseX, (int) mouseY));
	}

	private void renderSlotOverlay(MatrixStack matrixStack, Slot slot, int slotColor) {
		RenderSystem.disableDepthTest();
		int xPos = slot.xPos;
		int yPos = slot.yPos;
		RenderSystem.colorMask(true, true, true, false);
		fillGradient(matrixStack, xPos, yPos, xPos + 16, yPos + 16, slotColor, slotColor);
		RenderSystem.colorMask(true, true, true, true);
		RenderSystem.enableDepthTest();
	}

	@Override
	public boolean keyPressed(int keyCode, int scanCode, int modifiers) {
		if (keyCode == 256) {
			PacketHandler.sendToServer(new BackpackOpenMessage());
			return true;
		}
		return super.keyPressed(keyCode, scanCode, modifiers);
	}

	@Override
	protected void renderHoveredTooltip(MatrixStack matrixStack, int x, int y) {
		super.renderHoveredTooltip(matrixStack, x, y);
		GuiHelper.renderTooltip(minecraft, matrixStack, x, y);
	}

	public static SettingsScreen constructScreen(SettingsContainer settingsContainer, PlayerInventory playerInventory, ITextComponent title) {
		return new SettingsScreen(settingsContainer, playerInventory, title);
	}

	public BackpackSettingsTabControl getSettingsTabControl() {
		return settingsTabControl;
	}
}
