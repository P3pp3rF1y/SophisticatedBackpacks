package net.p3pp3rf1y.sophisticatedcore.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class StorageInsertMessage {
	private final int slotIndex;

	public StorageInsertMessage(int slotIndex) {
		this.slotIndex = slotIndex;
	}

	public static void encode(StorageInsertMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
	}

	public static StorageInsertMessage decode(FriendlyByteBuf packetBuffer) {
		return new StorageInsertMessage(packetBuffer.readInt());
	}

	static void onMessage(StorageInsertMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, StorageInsertMessage msg) {
		if (player == null) {
			return;
		}

		AbstractContainerMenu containerMenu = player.containerMenu;
		ItemStack storageStack = containerMenu.getSlot(msg.slotIndex).getItem();
		if (storageStack.getItem() instanceof IStashStorageItem stashStorageItem) {
			ItemStack heldItem = containerMenu.getCarried();
			containerMenu.setCarried(stashStorageItem.stash(storageStack, heldItem));
		}
	}
}
