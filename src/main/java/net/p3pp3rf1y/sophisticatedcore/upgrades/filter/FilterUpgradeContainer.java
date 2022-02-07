package net.p3pp3rf1y.sophisticatedcore.upgrades.filter;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class FilterUpgradeContainer extends UpgradeContainerBase<FilterUpgradeWrapper, FilterUpgradeContainer> {
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> BASIC_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	public static final UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> ADVANCED_TYPE = new UpgradeContainerType<>(FilterUpgradeContainer::new);
	private static final String DATA_DIRECTION = "direction";
	private final ContentsFilterLogicContainer filterLogicContainer;

	private FilterUpgradeContainer(Player player, int containerId, FilterUpgradeWrapper wrapper, UpgradeContainerType<FilterUpgradeWrapper, FilterUpgradeContainer> type) {
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
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_DIRECTION, direction));
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (filterLogicContainer.handleMessage(data)) {
			return;
		}

		if (data.contains(DATA_DIRECTION)) {
			setDirection(Direction.fromName(data.getString(DATA_DIRECTION)));
		}
	}
}
