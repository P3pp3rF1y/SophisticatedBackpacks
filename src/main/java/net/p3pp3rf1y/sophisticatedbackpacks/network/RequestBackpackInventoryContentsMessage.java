package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fmllegacy.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackUpgradeHandler;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class RequestBackpackInventoryContentsMessage {
	private final UUID backpackUuid;

	public RequestBackpackInventoryContentsMessage(UUID backpackUuid) {
		this.backpackUuid = backpackUuid;
	}

	public static void encode(RequestBackpackInventoryContentsMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
	}

	public static RequestBackpackInventoryContentsMessage decode(FriendlyByteBuf packetBuffer) {
		return new RequestBackpackInventoryContentsMessage(packetBuffer.readUUID());
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

		CompoundTag backpackContents = BackpackStorage.get().getOrCreateBackpackContents(msg.backpackUuid);

		CompoundTag inventoryContents = new CompoundTag();
		Tag inventoryNbt = backpackContents.get(BackpackInventoryHandler.INVENTORY_TAG);
		if (inventoryNbt != null) {
			inventoryContents.put(BackpackInventoryHandler.INVENTORY_TAG, inventoryNbt);
		}
		Tag upgradeNbt = backpackContents.get(BackpackUpgradeHandler.UPGRADE_INVENTORY_TAG);
		if (upgradeNbt != null) {
			inventoryContents.put(BackpackUpgradeHandler.UPGRADE_INVENTORY_TAG, upgradeNbt);
		}

		PacketHandler.sendToClient(player, new BackpackContentsMessage(msg.backpackUuid, inventoryContents));
	}
}
