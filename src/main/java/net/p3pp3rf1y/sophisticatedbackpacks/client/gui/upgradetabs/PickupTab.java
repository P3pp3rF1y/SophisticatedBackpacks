package net.p3pp3rf1y.sophisticatedbackpacks.client.gui.upgradetabs;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.PickupUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.function.Consumer;

public class PickupTab extends UpgradeSettingsTab<PickupUpgradeContainer> {
	private final int slotsLeftX;
	private final int slotsTopY;
	private final Consumer<PickupTab> onOpen;
	private final Consumer<PickupTab> onClose;
	private final BackpackScreen screen;
	private static final TextureBlitData SLOT_BACKGROUND = new TextureBlitData(new ResourceLocation(SophisticatedBackpacks.MOD_ID, "textures/gui/backpack_54.png"), 7, 17, 54, 54);

	public PickupTab(PickupUpgradeContainer upgradeContainer, int x, int y, TabDimensions openTabDimensions, BackpackScreen screen, Consumer<PickupTab> onOpen, Consumer<PickupTab> onClose) {
		super(upgradeContainer, x, y, openTabDimensions, new ItemStack(ModItems.PICKUP_UPGRADE), new TranslationTextComponent("gui.sophisticatedbackpacks.upgrades.pickup"),
				new TranslationTextComponent("gui.sophisticatedbackpacks.upgrades.pickup.tooltip"));
		this.screen = screen;
		this.onOpen = onOpen;
		this.onClose = onClose;
		ToggleButton<Boolean> blockAllowButton = new ToggleButton<>(x + 3, y + 24, 18, 18, button -> {
			getContainer().setAllowList(!getContainer().isAllowList());
			return true;
		},
				new TextureBlitData(UPGRADE_CONTROLS, 29, 0, 18, 18),
				ImmutableMap.of(
						true, new ToggleButton.StateData(new TextureBlitData(UPGRADE_CONTROLS, 1, 1, 256, 256, 32, 32, 16, 16),
								new TranslationTextComponent("gui.sophisticatedbackpacks.upgrades.buttons.allow")
						),
						false, new ToggleButton.StateData(new TextureBlitData(UPGRADE_CONTROLS, 1, 1, 256, 256, 48, 32, 16, 16),
								new TranslationTextComponent("gui.sophisticatedbackpacks.upgrades.buttons.block")
						)
				),
				() -> getContainer().isAllowList());
		blockAllowButton.setHoveredBackgroundTexture(new TextureBlitData(UPGRADE_CONTROLS, 47, 0, 18, 18));
		addHideableChild(blockAllowButton);
		slotsLeftX = x + 4;
		slotsTopY = y + 46;
	}

	@Override
	protected void onTabOpen() {
		super.onTabOpen();
		moveSlotsToTab();

		onOpen.accept(this);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (isOpen()) {
			GuiHelper.blit(minecraft, matrixStack, slotsLeftX - 1, slotsTopY - 1, SLOT_BACKGROUND);
		}
	}

	private void moveSlotsToTab() {
		int upgradeSlotNumber = 0;
		for (Slot slot : getContainer().getSlots()) {
			slot.xPos = slotsLeftX - screen.getGuiLeft() + (upgradeSlotNumber % 3) * 18;
			slot.yPos = slotsTopY - screen.getGuiTop() + (upgradeSlotNumber / 3) * 18;
			upgradeSlotNumber++;
		}
	}

	private void moveSlotsOutOfView() {
		getContainer().getSlots().forEach(slot -> {
			slot.xPos = -100;
			slot.yPos = -100;
		});

	}

	@Override
	protected void onTabClose() {
		super.onTabClose();

		moveSlotsOutOfView();
		onClose.accept(this);
	}

	public static class SecondTier extends PickupTab {
		public SecondTier(PickupUpgradeContainer upgradeContainer, int x, int y, BackpackScreen screen, Consumer<PickupTab> onOpen, Consumer<PickupTab> onClose) {
			super(upgradeContainer, x, y, new TabDimensions(63, 105), screen, onOpen, onClose);
		}

		@Override
		protected void renderWidget(MatrixStack matrixStack, int mouseX, int mouseY, float partialTicks) {
			super.renderWidget(matrixStack, mouseX, mouseY, partialTicks);
		}
	}
}
