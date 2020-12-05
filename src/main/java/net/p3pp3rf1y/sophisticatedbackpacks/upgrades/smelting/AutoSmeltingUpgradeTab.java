package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

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
public class AutoSmeltingUpgradeTab extends UpgradeSettingsTab<AutoSmeltingUpgradeContainer> {
	private final FilterLogicControl inputFilterLogicControl;
	private final FilterLogicControl fuelFilterLogicControl;
	private final SmeltingLogicControl smeltingLogicControl;

	public AutoSmeltingUpgradeTab(AutoSmeltingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(81, 167), screen, new TranslationTextComponent(translUpgrade("auto_smelting")),
				new TranslationTextComponent(translUpgradeTooltip("auto_smelting")));
		inputFilterLogicControl = addHideableChild(new FilterLogicControl.Advanced(new Position(x + 3, y + 24), getContainer().getInputFilterLogicContainer(), 4));
		smeltingLogicControl = addHideableChild(new SmeltingLogicControl(new Position(x + 3, y + 84), getContainer().getSmeltingLogicContainer()));
		fuelFilterLogicControl = addHideableChild(new FilterLogicControl(new Position(x + 3, y + 142), getContainer().getFuelFilterLogicContainer(), 4));
	}

	@Override
	protected void moveSlotsToTab() {
		inputFilterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
		smeltingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
		fuelFilterLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
