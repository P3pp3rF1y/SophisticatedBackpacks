package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;

public class UpgradeContainerType<W extends IUpgradeWrapper, T extends UpgradeContainerBase<W>> {
	private final IFactory<W, T> factory;

	public UpgradeContainerType(IFactory<W, T> factory) {
		this.factory = factory;
	}

	public T create(int containerId, W wrapper, boolean isClientSide) {
		return factory.create(containerId, wrapper, isClientSide);
	}

	public interface IFactory<W extends IUpgradeWrapper, T extends UpgradeContainerBase<W>> {
		T create(int containerId, W upgrade, boolean isClientSide);
	}
}
