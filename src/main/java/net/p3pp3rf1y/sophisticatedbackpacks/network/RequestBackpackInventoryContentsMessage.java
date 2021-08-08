package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.INBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
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

	public static void encode(RequestBackpackInventoryContentsMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
	}

	public static RequestBackpackInventoryContentsMessage decode(PacketBuffer packetBuffer) {
		return new RequestBackpackInventoryContentsMessage(packetBuffer.readUUID());
	}

	static void onMessage(RequestBackpackInventoryContentsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, RequestBackpackInventoryContentsMessage msg) {
		if (player == null) {
			return;
		}

		CompoundNBT backpackContents = BackpackStorage.get().getOrCreateBackpackContents(msg.backpackUuid);

		CompoundNBT inventoryContents = new CompoundNBT();
		INBT inventoryNbt = backpackContents.get(BackpackInventoryHandler.INVENTORY_TAG);
		if (inventoryNbt != null) {
			inventoryContents.put(BackpackInventoryHandler.INVENTORY_TAG, inventoryNbt);
		}
		INBT upgradeNbt = backpackContents.get(BackpackUpgradeHandler.UPGRADE_INVENTORY_TAG);
		if (upgradeNbt != null) {
			inventoryContents.put(BackpackUpgradeHandler.UPGRADE_INVENTORY_TAG, upgradeNbt);
		}

		PacketHandler.sendToClient(player, new BackpackContentsMessage(msg.backpackUuid, inventoryContents));
	}
}
