package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgradeContainer;

public class PickupUpgradeContainer extends UpgradeContainerBase<PickupUpgradeWrapper, PickupUpgradeContainer> implements IFilteredUpgradeContainer {
	public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> BASIC_TYPE = new UpgradeContainerType<>(PickupUpgradeContainer::new);
	public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> ADVANCED_TYPE = new UpgradeContainerType<>(PickupUpgradeContainer::new);
	private final FilterLogicContainer filterLogicContainer;

	private PickupUpgradeContainer(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide, UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> type) {
		super(containerId, wrapper, isClientSide, type);
		filterLogicContainer = new FilterLogicContainer(wrapper.getFilterLogic(), this, slots::add);
	}

	@Override
	public FilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		filterLogicContainer.handleMessage(data);
	}
}
