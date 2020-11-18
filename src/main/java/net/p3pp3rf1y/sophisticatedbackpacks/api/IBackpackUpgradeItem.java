package net.p3pp3rf1y.sophisticatedbackpacks.api;

public interface IBackpackUpgradeItem<T extends IUpgradeWrapper> {
	UpgradeType<T> getType();
}
