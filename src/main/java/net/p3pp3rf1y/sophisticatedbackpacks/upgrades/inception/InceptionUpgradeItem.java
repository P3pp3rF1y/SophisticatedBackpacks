package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, InceptionUpgradeItem> {
		public Wrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(upgrade, upgradeSaveHandler);
		}

		@Override
		public boolean displaysSettingsTab() {
			return Config.COMMON.inceptionUpgrade.upgradesUseInventoriesOfBackpacksInBackpack.get();
		}

		public InventoryOrder getInventoryOrder() {
			return NBTHelper.getEnumConstant(upgrade, "inventoryOrder", InventoryOrder::fromName).orElse(InventoryOrder.MAIN_FIRST);
		}

		public void setInventoryOrder(InventoryOrder inventoryOrder) {
			NBTHelper.setEnumConstant(upgrade, "inventoryOrder", inventoryOrder);
			save();
		}
	}
}
