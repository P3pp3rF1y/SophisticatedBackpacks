package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;

public interface IUpgradeWrapper {
	boolean isEnabled();

	void setEnabled(boolean enabled);

	default boolean canBeDisabled() {
		return true;
	}

	ItemStack getUpgradeStack();

	default boolean hideSettingsTab() {
		return false;
	}

	default void onBeforeRemoved() {
		//noop
	}
}
