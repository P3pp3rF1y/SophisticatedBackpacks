package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.matrix.MatrixStack;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.datafixers.util.Pair;
import net.minecraft.client.gui.screen.inventory.ContainerScreen;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

public class SlotSettingsScreen extends ContainerScreen<SlotSettingsContainer> {
	private SlotSettingsTabControl settingsTabControl;

	public SlotSettingsScreen(SlotSettingsContainer screenContainer, PlayerInventory inv, ITextComponent titleIn) {
		super(screenContainer, inv, titleIn);
		ySize = 114 + getContainer().getNumberOfRows() * 18;
		xSize = getContainer().getBackpackBackgroundProperties().getSlotsOnLine() * 18 + 14;
		playerInventoryTitleY = ySize - 94;
		playerInventoryTitleX = 8 + getContainer().getBackpackBackgroundProperties().getPlayerInventoryXOffset();
	}

	@Override
	protected void init() {
		super.init();

		settingsTabControl = new SlotSettingsTabControl(new Position(guiLeft + xSize, guiTop + 4));
		children.add(settingsTabControl);
	}

	@Override
	protected void drawGuiContainerBackgroundLayer(MatrixStack matrixStack, float partialTicks, int x, int y) {
		BackpackBackgroundProperties backpackBackgroundProperties = getContainer().getBackpackBackgroundProperties();
		BackpackGuiHelper.renderBackpackBackground(new Position((width - xSize) / 2, (height - ySize) / 2), matrixStack, getContainer().getBackpackInventorySlots().size(), backpackBackgroundProperties.getSlotsOnLine(), backpackBackgroundProperties.getTextureName(), xSize, minecraft);
	}

	@Override
	public void render(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		renderBackground(matrixStack);
		settingsTabControl.render(matrixStack, mouseX, mouseY, partialTicks);
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

	public static SlotSettingsScreen constructScreen(SlotSettingsContainer slotSettingsContainer, PlayerInventory playerInventory, ITextComponent title) {
		return new SlotSettingsScreen(slotSettingsContainer, playerInventory, title);
	}

	public SlotSettingsTabControl getSettingsTabControl() {
		return settingsTabControl;
	}
}
