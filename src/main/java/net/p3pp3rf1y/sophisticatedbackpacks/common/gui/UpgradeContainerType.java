package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;

public class UpgradeContainerType<W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>> {
	private final IFactory<W, C> factory;

	public UpgradeContainerType(IFactory<W, C> factory) {
		this.factory = factory;
	}

	public C create(int containerId, W wrapper, boolean isClientSide) {
		return factory.create(containerId, wrapper, isClientSide, this);
	}

	public interface IFactory<W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>> {
		C create(int containerId, W upgrade, boolean isClientSide, UpgradeContainerType<W, C> type);
	}
}
