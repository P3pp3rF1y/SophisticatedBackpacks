package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.item.crafting.BlastingRecipe;
import net.minecraft.world.item.crafting.SmeltingRecipe;
import net.minecraft.world.item.crafting.SmokingRecipe;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;

public abstract class CookingUpgradeTab<R extends AbstractCookingRecipe, W extends CookingUpgradeWrapper<W, ?, R>>
		extends UpgradeSettingsTab<CookingUpgradeContainer<R, W>> {
	private final CookingLogicControl<R> cookingLogicControl;

	protected CookingUpgradeTab(CookingUpgradeContainer<R, W> upgradeContainer, Position position, StorageScreenBase<?> screen, String tabLabel, String closedTooltip) {
		super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade(tabLabel), TranslationHelper.INSTANCE.translUpgradeTooltip(closedTooltip));
		cookingLogicControl = addHideableChild(new CookingLogicControl<R>(new Position(x + 3, y + 24), getContainer().getSmeltingLogicContainer()));
	}

	@Override
	protected void moveSlotsToTab() {
		cookingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}

	public static class SmeltingUpgradeTab extends CookingUpgradeTab<SmeltingRecipe, CookingUpgradeWrapper.SmeltingUpgradeWrapper> {
		public SmeltingUpgradeTab(CookingUpgradeContainer<SmeltingRecipe, CookingUpgradeWrapper.SmeltingUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, "smelting", "smelting");
		}
	}

	public static class SmokingUpgradeTab extends CookingUpgradeTab<SmokingRecipe, CookingUpgradeWrapper.SmokingUpgradeWrapper> {
		public SmokingUpgradeTab(CookingUpgradeContainer<SmokingRecipe, CookingUpgradeWrapper.SmokingUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, "smoking", "smoking");
		}
	}

	public static class BlastingUpgradeTab extends CookingUpgradeTab<BlastingRecipe, CookingUpgradeWrapper.BlastingUpgradeWrapper> {
		public BlastingUpgradeTab(CookingUpgradeContainer<BlastingRecipe, CookingUpgradeWrapper.BlastingUpgradeWrapper> upgradeContainer, Position position, StorageScreenBase<?> screen) {
			super(upgradeContainer, position, screen, "blasting", "blasting");
		}
	}
}
