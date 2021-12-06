package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.ClientBackpackContentsTooltip;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class BackpackContentsMessage {
	private final UUID backpackUuid;
	@Nullable
	private final CompoundTag backpackContents;

	public BackpackContentsMessage(UUID backpackUuid, @Nullable CompoundTag backpackContents) {
		this.backpackUuid = backpackUuid;
		this.backpackContents = backpackContents;
	}

	public static void encode(BackpackContentsMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
		packetBuffer.writeNbt(msg.backpackContents);
	}

	public static BackpackContentsMessage decode(FriendlyByteBuf packetBuffer) {
		return new BackpackContentsMessage(packetBuffer.readUUID(), packetBuffer.readNbt());
	}

	static void onMessage(BackpackContentsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(BackpackContentsMessage msg) {
		LocalPlayer player = Minecraft.getInstance().player;
		if (player == null || msg.backpackContents == null) {
			return;
		}

		BackpackStorage.get().setBackpackContents(msg.backpackUuid, msg.backpackContents);
		ClientBackpackContentsTooltip.refreshContents();
	}
}
