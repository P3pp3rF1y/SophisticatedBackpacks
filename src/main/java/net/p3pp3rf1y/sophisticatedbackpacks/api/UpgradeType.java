package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;

import java.util.function.Consumer;

public class UpgradeType<T extends IUpgradeWrapper> {
	private final IFactory<T> factory;

	public UpgradeType(IFactory<T> factory) {
		this.factory = factory;
	}

	public T create(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		return factory.create(backpackWrapper, upgrade, upgradeSaveHandler);
	}

	public interface IFactory<T extends IUpgradeWrapper> {
		T create(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler);
	}
}
