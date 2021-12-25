package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class VoidUpgradeContainer extends UpgradeContainerBase<VoidUpgradeWrapper, VoidUpgradeContainer> {
	private static final String DATA_SHOULD_WORKD_IN_GUI = "shouldWorkdInGUI";
	private static final String DATA_SHOULD_VOID_OVERFLOW = "shouldVoidOverflow";
	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public VoidUpgradeContainer(PlayerEntity player, int containerId, VoidUpgradeWrapper wrapper, UpgradeContainerType<VoidUpgradeWrapper, VoidUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(upgradeWrapper::getFilterLogic, this, slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SHOULD_WORKD_IN_GUI)) {
			setShouldWorkdInGUI(data.getBoolean(DATA_SHOULD_WORKD_IN_GUI));
		} else if (data.contains(DATA_SHOULD_VOID_OVERFLOW)) {
			setShouldVoidOverflow(data.getBoolean(DATA_SHOULD_VOID_OVERFLOW));
		}
		filterLogicContainer.handleMessage(data);
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setShouldWorkdInGUI(boolean shouldWorkdInGUI) {
		upgradeWrapper.setShouldWorkdInGUI(shouldWorkdInGUI);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_WORKD_IN_GUI, shouldWorkdInGUI));
	}

	public void setShouldVoidOverflow(boolean shouldVoidOverflow) {
		upgradeWrapper.setShouldVoidOverflow(shouldVoidOverflow);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHOULD_VOID_OVERFLOW, shouldVoidOverflow));
	}

	public boolean shouldWorkInGUI() {
		return upgradeWrapper.shouldWorkInGUI();
	}

	public boolean shouldVoidOverflow() {
		return upgradeWrapper.shouldVoidOverflow();
	}
}
