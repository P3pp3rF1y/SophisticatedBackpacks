package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;

import java.util.function.Consumer;

public class UpgradeContainerType<T extends UpgradeContainerBase> {
	private final IFactory<T> factory;

	public UpgradeContainerType(IFactory<T> factory) {
		this.factory = factory;
	}

	public T create(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide) {
		return factory.create(upgrade, upgradeSaveHandler, isClientSide);
	}

	public interface IFactory<T extends UpgradeContainerBase> {
		T create(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide);
	}
}
