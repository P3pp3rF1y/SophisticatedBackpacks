package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgradeContainer;

public abstract class PickupUpgradeContainer extends UpgradeContainerBase<PickupUpgradeWrapper> implements IFilteredUpgradeContainer {
	private final FilterLogicContainer filterLogicContainer;

	private PickupUpgradeContainer(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
		super(containerId, wrapper, isClientSide);
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

	public static class Basic extends PickupUpgradeContainer {
		public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Basic::new);

		private Basic(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> getType() {
			return TYPE;
		}
	}

	public static class Advanced extends PickupUpgradeContainer {
		public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Advanced::new);

		private Advanced(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> getType() {
			return TYPE;
		}
	}
}
