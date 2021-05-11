package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class ToolSwapperUpgradeContainer extends UpgradeContainerBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> {
	private static final String DATA_SHOULD_SWAP_WEAPON = "shouldSwapWeapon";
	private static final String DATA_SHOULD_SWAP_TOOLS = "shouldSwapTools";
	private final FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> filterLogicContainer;

	public ToolSwapperUpgradeContainer(PlayerEntity player, int upgradeContainerId, ToolSwapperUpgradeWrapper upgradeWrapper, UpgradeContainerType<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		filterLogicContainer = new ToolSwapperFilterContainer(this, upgradeWrapper::getFilterLogic, slots::add);

	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SHOULD_SWAP_WEAPON)) {
			setSwapWeapon(data.getBoolean(DATA_SHOULD_SWAP_WEAPON));
		} else if (data.contains(DATA_SHOULD_SWAP_TOOLS)) {
			setSwapTools(data.getBoolean(DATA_SHOULD_SWAP_TOOLS));
		} else {
			filterLogicContainer.handleMessage(data);
		}
	}

	public FilterLogicContainerBase<ToolSwapperFilterLogic, ToolFilterSlot> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setSwapWeapon(boolean shouldSwapWeapon) {
		upgradeWrapper.setSwapWeapon(shouldSwapWeapon);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_SWAP_WEAPON, shouldSwapWeapon));
	}

	public boolean shouldSwapWeapon() {
		return upgradeWrapper.shouldSwapWeapon();
	}

	public void setSwapTools(boolean shouldSwapTools) {
		upgradeWrapper.setSwapTools(shouldSwapTools);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_SWAP_TOOLS, shouldSwapTools));
	}

	public boolean shouldSwapTools() {
		return upgradeWrapper.shouldSwapTools();
	}
}
