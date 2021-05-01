package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class DepositUpgradeContainer extends UpgradeContainerBase<DepositUpgradeWrapper, DepositUpgradeContainer> {
	private final DepositFilterLogicContainer filterLogicContainer;

	public DepositUpgradeContainer(PlayerEntity player, int containerId, DepositUpgradeWrapper wrapper, UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new DepositFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public DepositFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		filterLogicContainer.handleMessage(data);
	}
}
