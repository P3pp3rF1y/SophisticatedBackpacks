package net.p3pp3rf1y.sophisticatedcore.client.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageBackgroundProperties;
import net.p3pp3rf1y.sophisticatedcore.settings.StorageSettingsTabControl;

import javax.annotation.Nullable;
import java.util.Optional;

public abstract class SettingsScreen extends AbstractContainerScreen<SettingsContainer<?>> {
	private StorageSettingsTabControl settingsTabControl;

	public SettingsScreen(SettingsContainer screenContainer, Inventory inv, Component titleIn) {
		super(screenContainer, inv, titleIn);
		imageHeight = 114 + getMenu().getNumberOfRows() * 18;
		imageWidth = getMenu().getStorageBackgroundProperties().getSlotsOnLine() * 18 + 14;
		inventoryLabelY = imageHeight - 94;
		inventoryLabelX = 8 + getMenu().getStorageBackgroundProperties().getPlayerInventoryXOffset();
	}

	@Override
	protected void init() {
		super.init();

		settingsTabControl = initializeTabControl();
		addWidget(settingsTabControl);
	}

	protected abstract StorageSettingsTabControl initializeTabControl();

	@Override
	protected void renderBg(PoseStack matrixStack, float partialTicks, int x, int y) {
		StorageBackgroundProperties storageBackgroundProperties = getMenu().getStorageBackgroundProperties();
		StorageGuiHelper.renderStorageBackground(new Position((width - imageWidth) / 2, (height - imageHeight) / 2), matrixStack, getMenu().getStorageInventorySlots().size(), getMenu().getSlotsOnLine(), storageBackgroundProperties.getTextureName(), imageWidth, menu.getNumberOfRows());
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		menu.detectSettingsChangeAndReload();
		renderBackground(matrixStack);
		settingsTabControl.render(matrixStack, mouseX, mouseY, partialTicks);
		matrixStack.translate(0, 0, 200);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
		settingsTabControl.renderTooltip(this, matrixStack, mouseX, mouseY);
		renderTooltip(matrixStack, mouseX, mouseY);
	}

	@Override
	protected void renderLabels(PoseStack matrixStack, int mouseX, int mouseY) {
		super.renderLabels(matrixStack, mouseX, mouseY);
		for (int slotId = 0; slotId < menu.ghostSlots.size(); ++slotId) {
			Slot slot = menu.ghostSlots.get(slotId);
			renderSlot(matrixStack, slot);

			settingsTabControl.renderSlotOverlays(matrixStack, slot, this::renderSlotOverlay);

			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
				hoveredSlot = slot;
				renderSlotOverlay(matrixStack, slot, getSlotColor(slotId));
			}
		}
	}

	@Override
	protected void renderSlot(PoseStack poseStack, Slot slot) {
		Optional<ItemStack> memorizedStack = getMenu().getMemorizedStackInSlot(slot.getSlotIndex());
		ItemStack itemstack = slot.getItem();
		if (memorizedStack.isPresent()) {
			itemstack = memorizedStack.get();
		}

		setBlitOffset(100);
		itemRenderer.blitOffset = 100.0F;

		RenderSystem.enableDepthTest();
		poseStack.pushPose();
		settingsTabControl.renderGuiItem(itemRenderer, itemstack, slot);
		poseStack.popPose();
		itemRenderer.blitOffset = 0.0F;
		setBlitOffset(0);

		if (memorizedStack.isPresent()) {
			drawMemorizedStackOverlay(poseStack, slot.x, slot.y);
		}
	}

	private void drawMemorizedStackOverlay(PoseStack poseStack, int x, int y) {
		poseStack.pushPose();
		RenderSystem.enableBlend();
		RenderSystem.disableDepthTest();
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderTexture(0, GuiHelper.GUI_CONTROLS);
		blit(poseStack, x, y, 77, 0, 16, 16);
		RenderSystem.enableDepthTest();
		RenderSystem.disableBlend();
		poseStack.popPose();
	}

	@SuppressWarnings("java:S2589") // slot can actually be null despite being marked non null
	@Override
	protected void slotClicked(Slot slot, int slotId, int mouseButton, ClickType type) {
		//noinspection ConstantConditions
		if (slot != null) {
			settingsTabControl.handleSlotClick(slot, mouseButton);
		}
	}

	@Override
	public boolean mouseDragged(double mouseX, double mouseY, int button, double dragX, double dragY) {
		Slot slot = findSlot(mouseX, mouseY);
		if (slot != null) {
			settingsTabControl.handleSlotClick(slot, button);
		}
		return true;
	}

	@Nullable
	@Override
	protected Slot findSlot(double mouseX, double mouseY) {
		for (int i = 0; i < menu.ghostSlots.size(); ++i) {
			Slot slot = menu.ghostSlots.get(i);
			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
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

	private void renderSlotOverlay(PoseStack matrixStack, Slot slot, int slotColor) {
		renderSlotOverlay(matrixStack, slot.x, slot.y, 16, slotColor);
	}

	private void renderSlotOverlay(PoseStack matrixStack, int xPos, int yPos, int height, int slotColor) {
		RenderSystem.disableDepthTest();
		RenderSystem.colorMask(true, true, true, false);
		fillGradient(matrixStack, xPos, yPos, xPos + 16, yPos + height, slotColor, slotColor);
		RenderSystem.colorMask(true, true, true, true);
		RenderSystem.enableDepthTest();
	}

	@Override
	public boolean keyPressed(int keyCode, int scanCode, int modifiers) {
		if (keyCode == 256) {
			sendStorageInventoryScreenOpenMessage();
			return true;
		}
		return super.keyPressed(keyCode, scanCode, modifiers);
	}

	protected abstract void sendStorageInventoryScreenOpenMessage();

	public StorageSettingsTabControl getSettingsTabControl() {
		return settingsTabControl;
	}
}
