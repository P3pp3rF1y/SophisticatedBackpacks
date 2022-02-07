package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;

public class DepositUpgradeContainer extends UpgradeContainerBase<DepositUpgradeWrapper, DepositUpgradeContainer> {
	private final DepositFilterLogicContainer filterLogicContainer;

	public DepositUpgradeContainer(Player player, int containerId, DepositUpgradeWrapper wrapper, UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new DepositFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public DepositFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		filterLogicContainer.handleMessage(data);
	}
}
