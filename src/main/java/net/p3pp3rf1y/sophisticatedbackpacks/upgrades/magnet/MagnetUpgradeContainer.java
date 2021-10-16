package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogicContainer;

public class MagnetUpgradeContainer extends UpgradeContainerBase<MagnetUpgradeWrapper, MagnetUpgradeContainer> {
	private static final String DATA_PICKUP_ITEMS = "pickupItems";
	private static final String DATA_PICKUP_XP = "pickupXp";

	private final ContentsFilterLogicContainer filterLogicContainer;

	public MagnetUpgradeContainer(PlayerEntity player, int containerId, MagnetUpgradeWrapper wrapper, UpgradeContainerType<MagnetUpgradeWrapper, MagnetUpgradeContainer> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new ContentsFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_PICKUP_ITEMS)) {
			setPickupItems(data.getBoolean(DATA_PICKUP_ITEMS));
		} else if (data.contains(DATA_PICKUP_XP)) {
			setPickupXp(data.getBoolean(DATA_PICKUP_XP));
		}
		filterLogicContainer.handleMessage(data);
	}

	public ContentsFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setPickupItems(boolean pickupItems) {
		upgradeWrapper.setPickupItems(pickupItems);
		sendBooleanToServer(DATA_PICKUP_ITEMS, pickupItems);
	}

	public boolean shouldPickupItems() {
		return upgradeWrapper.shouldPickupItems();
	}

	public void setPickupXp(boolean pickupXp) {
		upgradeWrapper.setPickupXp(pickupXp);
		sendBooleanToServer(DATA_PICKUP_XP, pickupXp);
	}

	public boolean shouldPickupXp() {
		return upgradeWrapper.shouldPickupXp();
	}
}
