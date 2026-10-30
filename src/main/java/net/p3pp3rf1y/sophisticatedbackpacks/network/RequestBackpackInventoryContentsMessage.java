package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import javax.annotation.Nullable;

import java.util.UUID;
import java.util.function.Supplier;

public class RequestBackpackInventoryContentsMessage {
	private final UUID backpackUuid;
	private final ItemStackReference itemReference;

	public RequestBackpackInventoryContentsMessage(UUID backpackUuid, ItemStackReference itemReference) {
		this.backpackUuid = backpackUuid;
		this.itemReference = itemReference;
	}

	public static void encode(RequestBackpackInventoryContentsMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
		ItemStackReference.encode(msg.itemReference, packetBuffer);
	}

	public static RequestBackpackInventoryContentsMessage decode(FriendlyByteBuf packetBuffer) {
		return new RequestBackpackInventoryContentsMessage(packetBuffer.readUUID(), ItemStackReference.decode(packetBuffer));
	}

	static void onMessage(RequestBackpackInventoryContentsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, RequestBackpackInventoryContentsMessage msg) {
		if (player == null) {
			return;
		}

		ItemStackReference reference = msg.itemReference;
		ItemStack stack = reference.resolve(player);
		if (reference.containerId() == ItemStackReference.EQUIPMENT_HANDLER) {
			stack = PlayerInventoryProvider.get().getPlayerInventoryHandler(reference.handlerName())
					.filter(handler -> handler.getIdentifiers(player).contains(reference.identifier()) && reference.slot() >= 0
							&& reference.slot() < handler.getSlotCount(player, reference.identifier()))
					.map(handler -> handler.getStackInSlot(player, reference.identifier(), reference.slot())).orElse(ItemStack.EMPTY);
		}
		if (!(stack.getItem() instanceof BackpackItem) || LinkedStorageStackData.getEndpoint(stack) != null
				|| new BackpackWrapper(stack).getContentsUuid().filter(msg.backpackUuid::equals).isEmpty()) {
			return;
		}
		CompoundTag backpackContents = BackpackStorage.get().getBackpackContents(msg.backpackUuid).orElse(null);
		if (backpackContents == null) {
			return;
		}

		CompoundTag inventoryContents = new CompoundTag();
		Tag inventoryNbt = backpackContents.get(InventoryHandler.INVENTORY_TAG);
		if (inventoryNbt != null) {
			inventoryContents.put(InventoryHandler.INVENTORY_TAG, inventoryNbt);
		}
		Tag upgradeNbt = backpackContents.get(UpgradeHandler.UPGRADE_INVENTORY_TAG);
		if (upgradeNbt != null) {
			inventoryContents.put(UpgradeHandler.UPGRADE_INVENTORY_TAG, upgradeNbt);
		}

		SBPPacketHandler.INSTANCE.sendToClient(player, new BackpackContentsMessage(msg.backpackUuid, inventoryContents));
	}
}
