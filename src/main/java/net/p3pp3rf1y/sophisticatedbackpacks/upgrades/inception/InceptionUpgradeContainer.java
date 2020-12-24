package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class InceptionUpgradeContainer extends UpgradeContainerBase<InceptionUpgradeItem.Wrapper, InceptionUpgradeContainer> {
	private static final String DATA_INVENTORY_ORDER = "inventoryOrder";

	public InceptionUpgradeContainer(PlayerEntity player, int upgradeContainerId, InceptionUpgradeItem.Wrapper upgradeWrapper, UpgradeContainerType<InceptionUpgradeItem.Wrapper, InceptionUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_INVENTORY_ORDER)) {
			setInventoryOrder(InventoryOrder.fromName(data.getString(DATA_INVENTORY_ORDER)));
		}
	}

	public InventoryOrder getInventoryOrder() {
		return upgradeWrapper.getInventoryOrder();
	}

	public void setInventoryOrder(InventoryOrder inventoryOrder) {
		upgradeWrapper.setInventoryOrder(inventoryOrder);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_INVENTORY_ORDER, inventoryOrder));
	}
}
