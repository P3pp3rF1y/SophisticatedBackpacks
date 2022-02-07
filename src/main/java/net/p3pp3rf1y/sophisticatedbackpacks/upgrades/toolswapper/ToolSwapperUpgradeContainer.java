package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class ToolSwapperUpgradeContainer extends UpgradeContainerBase<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> {
	private static final String DATA_SHOULD_SWAP_WEAPON = "shouldSwapWeapon";
	private static final String DATA_TOOL_SWAP_MODE = "toolSwapMode";
	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public ToolSwapperUpgradeContainer(Player player, int upgradeContainerId, ToolSwapperUpgradeWrapper upgradeWrapper, UpgradeContainerType<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(upgradeWrapper::getFilterLogic, this, slots::add);

	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_SHOULD_SWAP_WEAPON)) {
			setSwapWeapon(data.getBoolean(DATA_SHOULD_SWAP_WEAPON));
		} else if (data.contains(DATA_TOOL_SWAP_MODE)) {
			setToolSwapMode(ToolSwapMode.fromName(data.getString(DATA_TOOL_SWAP_MODE)));
		} else {
			filterLogicContainer.handleMessage(data);
		}
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setSwapWeapon(boolean shouldSwapWeapon) {
		upgradeWrapper.setSwapWeapon(shouldSwapWeapon);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundTag(), DATA_SHOULD_SWAP_WEAPON, shouldSwapWeapon));
	}

	public boolean shouldSwapWeapon() {
		return upgradeWrapper.shouldSwapWeapon();
	}

	public void setToolSwapMode(ToolSwapMode toolSwapMode) {
		upgradeWrapper.setToolSwapMode(toolSwapMode);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_TOOL_SWAP_MODE, toolSwapMode));
	}

	public ToolSwapMode getToolSwapMode() {
		return upgradeWrapper.getToolSwapMode();
	}
}
