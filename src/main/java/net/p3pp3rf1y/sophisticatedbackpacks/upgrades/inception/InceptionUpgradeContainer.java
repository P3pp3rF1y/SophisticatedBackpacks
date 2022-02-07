package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class InceptionUpgradeContainer extends UpgradeContainerBase<InceptionUpgradeWrapper, InceptionUpgradeContainer> {
	private static final String DATA_INVENTORY_ORDER = "inventoryOrder";

	public InceptionUpgradeContainer(Player player, int upgradeContainerId, InceptionUpgradeWrapper upgradeWrapper, UpgradeContainerType<InceptionUpgradeWrapper, InceptionUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_INVENTORY_ORDER)) {
			setInventoryOrder(InventoryOrder.fromName(data.getString(DATA_INVENTORY_ORDER)));
		}
	}

	public InventoryOrder getInventoryOrder() {
		return upgradeWrapper.getInventoryOrder();
	}

	public void setInventoryOrder(InventoryOrder inventoryOrder) {
		upgradeWrapper.setInventoryOrder(inventoryOrder);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_INVENTORY_ORDER, inventoryOrder));
	}
}
