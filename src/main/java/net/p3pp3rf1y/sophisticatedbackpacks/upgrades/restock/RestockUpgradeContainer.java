package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class RestockUpgradeContainer extends UpgradeContainerBase<RestockUpgradeWrapper, RestockUpgradeContainer> {
	private final RestockFilterLogicContainer filterLogicContainer;

	public RestockUpgradeContainer(PlayerEntity player, int containerId, RestockUpgradeWrapper wrapper, UpgradeContainerType<RestockUpgradeWrapper, RestockUpgradeContainer> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new RestockFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public RestockFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		filterLogicContainer.handleMessage(data);
	}
}
