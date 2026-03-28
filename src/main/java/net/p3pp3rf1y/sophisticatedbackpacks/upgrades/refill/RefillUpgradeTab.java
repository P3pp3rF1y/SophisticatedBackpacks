package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.Component;
import net.minecraft.world.item.DyeColor;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.entity.player.ItemTooltipEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedcore.client.gui.IForegroundRenderable;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;

import java.util.ArrayList;
import java.util.List;

public abstract class RefillUpgradeTab extends UpgradeSettingsTab<RefillUpgradeContainer> implements IForegroundRenderable {
	private static final Component SCROLL_TOOLTIP = BackpackTranslationHelper.INSTANCE.translUpgrade("refill.scroll.tooltip").withStyle(ChatFormatting.ITALIC, ChatFormatting.DARK_GRAY);
	private final RefillFilterLogicControl filterLogicControl;
	private int slotBeingChanged = -1;
	private RefillUpgradeWrapper.TargetSlot targetSlotBeingChanged = null;

	private static List<Component> additionalTooltip = new ArrayList<>();

	static {
		NeoForge.EVENT_BUS.addListener(RefillUpgradeTab::addToTooltip);
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
		super(upgradeContainer, position, screen, BackpackTranslationHelper.INSTANCE.translUpgrade(upgradeName), BackpackTranslationHelper.INSTANCE.translUpgradeTooltip(upgradeName));

		filterLogicControl = addHideableChild(new RefillFilterLogicControl(screen, slotsInRow));
	}

	@Override
	protected void moveSlotsToTab() {
		filterLogicControl.moveSlotsToView();
	}

	@Override
	public void extractRenderState(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
		if (!shouldRender.getAsBoolean()) {
			return;
		}
		super.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);

		if (!filterLogicControl.isMouseOver(mouseX, mouseY)) {
			resetAdditionalTooltip();
			if (slotBeingChanged > -1) {
				saveTargetSlot();
			}
		}
	}

	@Override
	public void extractForeground(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
		filterLogicControl.extractForeground(guiGraphics, mouseX, mouseY, partialTicks);
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

	private class RefillFilterLogicControl extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> implements IForegroundRenderable {
		private final int slotsInRow;

		public RefillFilterLogicControl(StorageScreenBase<?> screen, int slotsInRow) {
			super(screen, new Position(RefillUpgradeTab.this.x + 3, RefillUpgradeTab.this.y + 24), RefillUpgradeTab.this.getContainer().getFilterLogicContainer(), slotsInRow);
			this.slotsInRow = slotsInRow;
		}

		@Override
		protected void extractWidget(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
			super.extractWidget(guiGraphics, mouseX, mouseY, partialTicks);
			if (!getContainer().allowsTargetSlotSelection()) {
				return;
			}

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

		private void renderTargetSlotAcronyms(GuiGraphicsExtractor guiGraphics) {
			getContainer().getSlots().forEach(slot -> {
				if (!slot.getItem().isEmpty()) {
					int slotIndex = slot.getSlotIndex();
					RefillUpgradeWrapper.TargetSlot ts = getContainer().getTargetSlot(slotIndex);
					RefillUpgradeWrapper.TargetSlot targetSlot = slotBeingChanged == slotIndex ? targetSlotBeingChanged : ts;
					guiGraphics.text(font, targetSlot.getAcronym(),
							getX() + (slotIndex % slotsInRow) * 18 + 10, getY() + (slotIndex / slotsInRow) * 18 + 2, DyeColor.GREEN.getTextColor());
				}
			});
		}

		private void updateTooltip(RefillUpgradeWrapper.TargetSlot targetSlot) {
			resetAdditionalTooltip();
			additionalTooltip.add(BackpackTranslationHelper.INSTANCE.translUpgrade("refill.target_slot.tooltip", targetSlot.getDescription()).withStyle(ChatFormatting.GRAY));
			additionalTooltip.add(SCROLL_TOOLTIP);
		}

		@Override
		public void extractRenderState(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
			super.extractRenderState(guiGraphics, mouseX, mouseY, partialTicks);

			int slot = getSlot(mouseX, mouseY);

			if (slotBeingChanged > -1 && slotBeingChanged != slot) {
				saveTargetSlot();
			}
		}

		@Override
		public boolean mouseScrolled(double mouseX, double mouseY, double scrollX, double scrollY) {
			int slot = getSlot(mouseX, mouseY);

			if (slotBeingChanged == -1) {
				slotBeingChanged = slot;
				targetSlotBeingChanged = getContainer().getTargetSlot(slot);
			}

			targetSlotBeingChanged = scrollY > 0 ? targetSlotBeingChanged.next() : targetSlotBeingChanged.previous();

			return true;
		}

		private int getSlot(double mouseX, double mouseY) {
			return ((int) mouseX - getX()) / 18 + slotsInRow * (((int) mouseY - getY()) / 18);
		}

		@Override
		public void extractForeground(GuiGraphicsExtractor guiGraphics, int mouseX, int mouseY, float partialTicks) {
			if (!getContainer().allowsTargetSlotSelection() || !isOpen) {
				return;
			}

			renderTargetSlotAcronyms(guiGraphics);
		}
	}
}
