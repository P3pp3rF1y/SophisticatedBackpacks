package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;

public class UpgradeContainerType<T extends UpgradeContainerBase> {
	private final IFactory<T> factory;

	public UpgradeContainerType(IFactory<T> factory) {
		this.factory = factory;
	}

	public T create(ItemStack upgrade) {
		return factory.create(upgrade);
	}

	public interface IFactory<T extends UpgradeContainerBase> {
		T create(ItemStack upgrade);
	}
}
