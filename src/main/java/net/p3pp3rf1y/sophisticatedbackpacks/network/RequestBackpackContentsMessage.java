package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class RequestBackpackContentsMessage {
	private final UUID backpackUuid;

	public RequestBackpackContentsMessage(UUID backpackUuid) {
		this.backpackUuid = backpackUuid;
	}

	public static void encode(RequestBackpackContentsMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeUniqueId(msg.backpackUuid);
	}

	public static RequestBackpackContentsMessage decode(PacketBuffer packetBuffer) {
		return new RequestBackpackContentsMessage(packetBuffer.readUniqueId());
	}

	static void onMessage(RequestBackpackContentsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, RequestBackpackContentsMessage msg) {
		if (player == null) {
			return;
		}

		PacketHandler.sendToClient(player, new BackpackContentsMessage(msg.backpackUuid, BackpackStorage.get().getOrCreateBackpackContents(msg.backpackUuid)));
	}
}
