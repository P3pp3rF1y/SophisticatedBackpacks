package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class FilteredUpgradeContainer<W extends IUpgradeWrapper & IFilteredUpgrade> extends UpgradeContainerBase<W, FilteredUpgradeContainer<W>> {
	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public FilteredUpgradeContainer(Player player, int containerId, W wrapper, UpgradeContainerType<W, FilteredUpgradeContainer<W>> type) {
		super(player, containerId, wrapper, type);
		filterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		filterLogicContainer.handleMessage(data);
	}
}
