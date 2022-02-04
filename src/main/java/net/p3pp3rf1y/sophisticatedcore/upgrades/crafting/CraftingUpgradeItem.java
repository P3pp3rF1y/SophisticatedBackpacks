package net.p3pp3rf1y.sophisticatedcore.upgrades.crafting;

import net.minecraft.world.item.CreativeModeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

public class CraftingUpgradeItem extends UpgradeItemBase<CraftingUpgradeWrapper> {
	private static final UpgradeType<CraftingUpgradeWrapper> TYPE = new UpgradeType<>(CraftingUpgradeWrapper::new);

	public CraftingUpgradeItem(CreativeModeTab itemGroup) {super(itemGroup);}

	@Override
	public UpgradeType<CraftingUpgradeWrapper> getType() {
		return TYPE;
	}
}
