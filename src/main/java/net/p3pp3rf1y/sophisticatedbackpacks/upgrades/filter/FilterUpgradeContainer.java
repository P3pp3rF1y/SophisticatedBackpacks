package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class FilterUpgradeContainer extends UpgradeContainerBase<FilterUpgradeWrapper, FilterUpgradeContainer> {
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> BASIC_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> ADVANCED_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	private static final String DATA_DIRECTION = "direction";
	private final ContentsFilterLogicContainer filterLogicContainer;

	private FilterUpgradeContainer(PlayerEntity player, int containerId, FilterUpgradeWrapper wrapper, UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new ContentsFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public ContentsFilterLogicContainer getFilterLogicContainer() {
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
