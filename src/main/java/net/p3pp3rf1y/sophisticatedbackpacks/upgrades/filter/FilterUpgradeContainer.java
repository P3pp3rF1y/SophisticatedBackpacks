package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public abstract class FilterUpgradeContainer extends UpgradeContainerBase<FilterUpgradeWrapper> implements IFilteredUpgradeContainer {
	private static final String DATA_DIRECTION = "direction";
	private final FilterLogicContainer filterLogicContainer;

	private FilterUpgradeContainer(int containerId, FilterUpgradeWrapper wrapper, boolean isClientSide) {
		super(containerId, wrapper, isClientSide);
		filterLogicContainer = new FilterLogicContainer(wrapper.getFilterLogic(), this, slots::add);
	}

	@Override
	public FilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public Direction getDirection() {
		return upgradeWrapper.getDirection();
	}

	public void setDirection(Direction direction) {
		upgradeWrapper.setDirection(direction);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_DIRECTION, direction));
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (filterLogicContainer.handleMessage(data)) {
			return;
		}

		if (data.contains(DATA_DIRECTION)) {
			setDirection(Direction.fromName(data.getString(DATA_DIRECTION)));
		}
	}

	public static class Basic extends FilterUpgradeContainer {
		public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> TYPE = new UpgradeContainerType<>(Basic::new);

		private Basic(int containerId, FilterUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> getType() {
			return TYPE;
		}
	}

	public static class Advanced extends FilterUpgradeContainer {
		public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> TYPE = new UpgradeContainerType<>(Advanced::new);

		private Advanced(int containerId, FilterUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> getType() {
			return TYPE;
		}
	}
}
