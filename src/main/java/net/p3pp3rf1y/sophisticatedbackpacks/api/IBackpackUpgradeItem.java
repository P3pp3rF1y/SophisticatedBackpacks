package net.p3pp3rf1y.sophisticatedbackpacks.api;

public interface IBackpackUpgradeItem<T extends IUpgradeWrapper> {
	UpgradeType<T> getType();

	default boolean canAddUpgradeTo(IBackpackWrapper backpackWrapper, boolean firstLevelBackpack) {
		return true;
	}

	default boolean canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		return true;
	}
}
