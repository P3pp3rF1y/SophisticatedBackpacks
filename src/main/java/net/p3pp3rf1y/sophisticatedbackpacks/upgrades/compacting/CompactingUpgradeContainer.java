package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class CompactingUpgradeContainer extends UpgradeContainerBase<CompactingUpgradeWrapper, CompactingUpgradeContainer> {
	private static final String DATA_SHOULD_WORKD_IN_GUI = "shouldWorkdInGUI";
	private final FilterLogicContainer<FilterLogic> filterLogicContainer;
	private static final String DATA_SHOULD_COMPACT_NON_UNCRAFTABLE = "shouldCompactNonUncraftable";

	public CompactingUpgradeContainer(PlayerEntity player, int containerId, CompactingUpgradeWrapper wrapper, UpgradeContainerType<CompactingUpgradeWrapper, CompactingUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(upgradeWrapper::getFilterLogic, this, slots::add);
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setCompactNonUncraftable(boolean shouldCompactNonUncraftable) {
		upgradeWrapper.setCompactNonUncraftable(shouldCompactNonUncraftable);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_COMPACT_NON_UNCRAFTABLE, shouldCompactNonUncraftable));
	}

	public boolean shouldCompactNonUncraftable() {
		return upgradeWrapper.shouldCompactNonUncraftable();
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SHOULD_COMPACT_NON_UNCRAFTABLE)) {
			setCompactNonUncraftable(data.getBoolean(DATA_SHOULD_COMPACT_NON_UNCRAFTABLE));
		} else if (data.contains(DATA_SHOULD_WORKD_IN_GUI)) {
			setShouldWorkdInGUI(data.getBoolean(DATA_SHOULD_WORKD_IN_GUI));
		} else {
			filterLogicContainer.handleMessage(data);
		}
	}

	public void setShouldWorkdInGUI(boolean shouldWorkdInGUI) {
		upgradeWrapper.setShouldWorkdInGUI(shouldWorkdInGUI);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_WORKD_IN_GUI, shouldWorkdInGUI));
	}

	public boolean shouldWorkInGUI() {
		return upgradeWrapper.shouldWorkInGUI();
	}
}
