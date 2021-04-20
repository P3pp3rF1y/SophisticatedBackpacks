package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainerBase;

public class ToolSwapperUpgradeContainer extends UpgradeContainerBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> {
	private final FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> filterLogicContainer;

	public ToolSwapperUpgradeContainer(PlayerEntity player, int upgradeContainerId, ToolSwapperUpgradeWrapper upgradeWrapper, UpgradeContainerType<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		filterLogicContainer = new ToolSwapperFilterContainer(this, upgradeWrapper::getFilterLogic, slots::add);

	}

	@Override
	public void handleMessage(CompoundNBT data) {
		filterLogicContainer.handleMessage(data);
	}

	public FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> getFilterLogicContainer() {
		return filterLogicContainer;
	}
}
