package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;

import java.util.function.Consumer;

public class UpgradeType<T extends IUpgradeWrapper> {
	private final IFactory<T> factory;

	public UpgradeType(IFactory<T> factory) {
		this.factory = factory;
	}

	public T create(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		return factory.create(storageWrapper, upgrade, upgradeSaveHandler);
	}

	public interface IFactory<T extends IUpgradeWrapper> {
		T create(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler);
	}
}
