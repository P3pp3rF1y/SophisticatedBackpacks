package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.item.crafting.BlastingRecipe;
import net.minecraft.world.item.crafting.SmeltingRecipe;
import net.minecraft.world.item.crafting.SmokingRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeTooltip;

public abstract class AutoCookingUpgradeTab<R extends AbstractCookingRecipe, W extends AutoCookingUpgradeWrapper<W, ?, R>>
		extends UpgradeSettingsTab<AutoCookingUpgradeContainer<R, W>> {
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> inputFilterLogicControl;
	private final FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> fuelFilterLogicControl;
	private final CookingLogicControl<R> cookingLogicControl;

	protected AutoCookingUpgradeTab(AutoCookingUpgradeContainer<R, W> upgradeContainer, Position position, BackpackScreen screen, String tabLabel, String closedTooltip) {
		super(upgradeContainer, position, screen, translUpgrade(tabLabel), translUpgradeTooltip(closedTooltip));
		inputFilterLogicControl = addHideableChild(new FilterLogicControl.Advanced(screen, new Position(x + 3, y + 24), getContainer().getInputFilterLogicContainer(),
				Config.COMMON.autoSmeltingUpgrade.inputFilterSlotsInRow.get()));
		cookingLogicControl = addHideableChild(new CookingLogicControl<>(new Position(x + 3, y + 84), getContainer().getCookingLogicContainer()));
		fuelFilterLogicControl = addHideableChild(new FilterLogicControl<>(screen, new Position(x + 3, y + 142), getContainer().getFuelFilterLogicContainer(),
				Config.COMMON.autoSmeltingUpgrade.fuelFilterSlotsInRow.get()));
	}

	@Override
	protected void moveSlotsToTab() {
		inputFilterLogicControl.moveSlotsToView();
		cookingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
		fuelFilterLogicControl.moveSlotsToView();
	}

	public static class AutoSmeltingUpgradeTab extends AutoCookingUpgradeTab<SmeltingRecipe, AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> {
		public AutoSmeltingUpgradeTab(AutoCookingUpgradeContainer<SmeltingRecipe, AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, "auto_smelting", "auto_smelting");
		}
	}

	public static class AutoSmokingUpgradeTab extends AutoCookingUpgradeTab<SmokingRecipe, AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> {
		public AutoSmokingUpgradeTab(AutoCookingUpgradeContainer<SmokingRecipe, AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, "auto_smoking", "auto_smoking");
		}
	}

	public static class AutoBlastingUpgradeTab extends AutoCookingUpgradeTab<BlastingRecipe, AutoCookingUpgradeWrapper.AutoBlastingUpgradeWrapper> {
		public AutoBlastingUpgradeTab(AutoCookingUpgradeContainer<BlastingRecipe, AutoCookingUpgradeWrapper.AutoBlastingUpgradeWrapper> upgradeContainer, Position position, BackpackScreen screen) {
			super(upgradeContainer, position, screen, "auto_blasting", "auto_blasting");
		}
	}
}
