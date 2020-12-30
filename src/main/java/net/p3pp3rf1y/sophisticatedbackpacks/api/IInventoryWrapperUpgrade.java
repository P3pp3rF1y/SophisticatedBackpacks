package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IObservableItemHandler;

public interface IInventoryWrapperUpgrade {
	IObservableItemHandler wrapInventory(ItemStack backpack, IObservableItemHandler inventory);
}
