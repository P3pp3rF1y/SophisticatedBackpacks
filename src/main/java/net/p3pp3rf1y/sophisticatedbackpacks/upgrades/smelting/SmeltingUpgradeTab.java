package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

@OnlyIn(Dist.CLIENT)
public class SmeltingUpgradeTab extends UpgradeSettingsTab<SmeltingUpgradeContainer> {
	private final SmeltingLogicControl smeltingLogicControl;

	public SmeltingUpgradeTab(SmeltingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(78, 85), screen, new TranslationTextComponent(translUpgrade("smelting")),
				new TranslationTextComponent(translUpgradeTooltip("smelting")));
		smeltingLogicControl = addHideableChild(new SmeltingLogicControl(new Position(x + 3, y + 24), getContainer().getSmeltingLogicContainer()));
	}

	@Override
	protected void moveSlotsToTab() {
		smeltingLogicControl.moveSlotsToView(screen.getGuiLeft(), screen.getGuiTop());
	}
}
