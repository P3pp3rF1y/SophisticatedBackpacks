package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.DyeColor;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.entity.player.ItemTooltipEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;

import java.util.ArrayList;
import java.util.List;

public abstract class RefillUpgradeTab extends UpgradeSettingsTab<RefillUpgradeContainer> {
	private static final Component SCROLL_TOOLTIP = SBPTranslationHelper.INSTANCE.translUpgrade("refill.scroll.tooltip").withStyle(ChatFormatting.ITALIC, ChatFormatting.DARK_GRAY);
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> filterLogicControl;
	private int slotBeingChanged = -1;
	private RefillUpgradeWrapper.TargetSlot targetSlotBeingChanged = null;

	private static List<Component> additionalTooltip = new ArrayList<>();
	static {
		MinecraftForge.EVENT_BUS.addListener(RefillUpgradeTab::addToTooltip);
	}

	private static void addToTooltip(ItemTooltipEvent event) {
		if (!additionalTooltip.isEmpty()) {
			LocalPlayer player = Minecraft.getInstance().player;
			if (player != null && player.containerMenu instanceof BackpackContainer) {
				event.getToolTip().addAll(additionalTooltip);
			}
		}
	}

	protected RefillUpgradeTab(RefillUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsInRow, String upgradeName) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade(upgradeName), SBPTranslationHelper.INSTANCE.translUpgradeTooltip(upgradeName));

		filterLogicControl = addHideableChild(new RefillFilterLogicControl(screen, slotsInRow));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.getAsBoolean()) {
			return;
		}
		super.render(matrixStack, mouseX, mouseY, partialTicks);

		if (!filterLogicControl.isMouseOver(mouseX, mouseY)) {
			resetAdditionalTooltip();
			if (slotBeingChanged > -1) {
				saveTargetSlot();
			}
		}
	}

	private void resetAdditionalTooltip() {
		if (!additionalTooltip.isEmpty()) {
			additionalTooltip = new ArrayList<>();
		}
	}

	@Override
	protected void onTabClose() {
		super.onTabClose();
		resetAdditionalTooltip();
	}

	private void saveTargetSlot() {
		getContainer().setTargetSlot(slotBeingChanged, targetSlotBeingChanged);
		slotBeingChanged = -1;
	}

	public static class Basic extends RefillUpgradeTab {
		public Basic(RefillUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsInRow) {
			super(upgradeContainer, position, screen, slotsInRow, "refill");
		}
	}

	public static class Advanced extends RefillUpgradeTab {
		public Advanced(RefillUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, int slotsInRow) {
			super(upgradeContainer, position, screen, slotsInRow, "advanced_refill");
		}
	}

	private class RefillFilterLogicControl extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> {
		private final int slotsInRow;

		public RefillFilterLogicControl(StorageScreenBase<?> screen, int slotsInRow) {
			super(screen, new Position(RefillUpgradeTab.this.x + 3, RefillUpgradeTab.this.y + 24), RefillUpgradeTab.this.getContainer().getFilterLogicContainer(), slotsInRow);
			this.slotsInRow = slotsInRow;
		}

		@Override
		protected void renderWidget(PoseStack poseStack, int mouseX, int mouseY, float partialTicks) {
			super.renderWidget(poseStack, mouseX, mouseY, partialTicks);
			if (!getContainer().allowsTargetSlotSelection()) {
				return;
			}

			renderTargetSlotAcronyms(poseStack);
			updateTargetSlotTooltip(mouseX, mouseY);
		}

		private void updateTargetSlotTooltip(int mouseX, int mouseY) {
			if (isMouseOver(mouseX, mouseY)) {
				int slot = getSlot(mouseX, mouseY);
				if (slotBeingChanged > -1) {
					updateTooltip(targetSlotBeingChanged);
				} else {
					RefillUpgradeWrapper.TargetSlot targetSlot = getContainer().getTargetSlot(slot);
					if (additionalTooltip.isEmpty() || !additionalTooltip.get(0).equals(targetSlot.getDescription())) {
						updateTooltip(targetSlot);
					}
				}
			}
		}

		private void renderTargetSlotAcronyms(PoseStack poseStack) {
			poseStack.pushPose();
			poseStack.translate(0, 0, 300);
			getContainer().getSlots().forEach(slot -> {
				if (!slot.getItem().isEmpty()) {
					int slotIndex = slot.getSlotIndex();
					RefillUpgradeWrapper.TargetSlot ts = getContainer().getTargetSlot(slotIndex);
					RefillUpgradeWrapper.TargetSlot targetSlot = slotBeingChanged == slotIndex ? targetSlotBeingChanged : ts;
					drawString(poseStack, font, targetSlot.getAcronym(),
							getX() + (slotIndex % slotsInRow) * 18 + 10, getY() + (slotIndex / slotsInRow) * 18 + 2, DyeColor.GREEN.getTextColor());
				}
					});

			poseStack.popPose();
		}

		private void updateTooltip(RefillUpgradeWrapper.TargetSlot targetSlot) {
			resetAdditionalTooltip();
			additionalTooltip.add(SBPTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.tooltip", targetSlot.getDescription()).withStyle(ChatFormatting.GRAY));
			additionalTooltip.add(SCROLL_TOOLTIP);
		}

		@Override
		public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
			super.render(matrixStack, mouseX, mouseY, partialTicks);

			int slot = getSlot(mouseX, mouseY);

			if (slotBeingChanged > -1 && slotBeingChanged != slot) {
				saveTargetSlot();
			}
		}

		@Override
		public boolean mouseScrolled(double mouseX, double mouseY, double pDelta) {
			int slot = getSlot(mouseX, mouseY);

			if (slotBeingChanged == -1) {
				slotBeingChanged = slot;
				targetSlotBeingChanged = getContainer().getTargetSlot(slot);
			}

			targetSlotBeingChanged = pDelta > 0 ? targetSlotBeingChanged.next() : targetSlotBeingChanged.previous();

			return true;
		}

		private int getSlot(double mouseX, double mouseY) {
			return ((int) mouseX - getX()) / 18 + slotsInRow * (((int) mouseY - getY()) / 18);
		}
	}
}
