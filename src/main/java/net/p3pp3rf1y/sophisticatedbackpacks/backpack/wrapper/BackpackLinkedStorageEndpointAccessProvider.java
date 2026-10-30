package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAccessProvider;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;

public class BackpackLinkedStorageEndpointAccessProvider implements ILinkedStorageEndpointAccessProvider {
	@Override
	public ItemStack resolveEndpointStack(ServerPlayer player, ItemStackReference reference) {
		if (reference.containerId() != ItemStackReference.EQUIPMENT_HANDLER) {
			return ItemStack.EMPTY;
		}
		return PlayerInventoryProvider.get().getPlayerInventoryHandler(reference.handlerName())
				.filter(handler -> handler.getIdentifiers(player).contains(reference.identifier()) && reference.slot() >= 0
						&& reference.slot() < handler.getSlotCount(player, reference.identifier()))
				.map(handler -> handler.getStackInSlot(player, reference.identifier(), reference.slot()))
				.filter(stack -> stack.getItem() instanceof BackpackItem).orElse(ItemStack.EMPTY);
	}
}
