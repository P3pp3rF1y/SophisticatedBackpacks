package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

@OnlyIn(Dist.CLIENT)
public abstract class PickupUpgradeTab extends UpgradeSettingsTab<PickupUpgradeContainer> {
	protected PickupUpgradeTab(PickupUpgradeContainer upgradeContainer, Position position, Dimension openTabDimension, BackpackScreen screen) {
		super(upgradeContainer, position, openTabDimension, screen);
	}

	public static class Basic extends PickupUpgradeTab {
		public Basic(PickupUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(63, 106), screen);
			addHideableChild(new FilterLogicControl(new Position(x + 3, y + 24), getContainer()));
		}

		@Override
		protected int getSlotsPerRow() {
			return 3;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("pickup"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("pickup"));
		}
	}

	public static class Advanced extends PickupUpgradeTab {
		public Advanced(PickupUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, new Dimension(81, 124), screen);

			addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer()));
		}

		@Override
		protected int getSlotsPerRow() {
			return 4;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("advanced_pickup"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("advanced_pickup"));
		}
	}
}
