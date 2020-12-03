package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class FilteredUpgradeContainer<W extends IUpgradeWrapper & IFilteredUpgrade> extends UpgradeContainerBase<W, FilteredUpgradeContainer<W>>
		implements IFilteredUpgradeContainer {
	protected final FilterLogicContainer filterLogicContainer;

	public FilteredUpgradeContainer(int containerId, W wrapper, boolean isClientSide, UpgradeContainerType<W, FilteredUpgradeContainer<W>> type) {
		super(containerId, wrapper, isClientSide, type);
		filterLogicContainer = new FilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
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
