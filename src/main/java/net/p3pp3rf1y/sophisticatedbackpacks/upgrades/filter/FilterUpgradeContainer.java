package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class FilterUpgradeContainer extends UpgradeContainerBase<FilterUpgradeWrapper, FilterUpgradeContainer> {
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> BASIC_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> ADVANCED_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	private static final String DATA_DIRECTION = "direction";
	private final FilterLogicContainer filterLogicContainer;

	private FilterUpgradeContainer(int containerId, FilterUpgradeWrapper wrapper, boolean isClientSide, UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> type) {
		super(containerId, wrapper, isClientSide, type);
		filterLogicContainer = new FilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

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
}
