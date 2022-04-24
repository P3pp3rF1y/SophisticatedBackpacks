package net.p3pp3rf1y.sophisticatedcore.api;

import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;

import java.util.Optional;

public interface IStashStorageItem {
	Optional<TooltipComponent> getInventoryTooltip(ItemStack stack);
	ItemStack stash(ItemStack storageStack, ItemStack stack);
	boolean isItemStashable(ItemStack storageStack, ItemStack stack);
}
