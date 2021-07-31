package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackTooltipRenderer;

import javax.annotation.Nullable;
import java.util.UUID;
import java.util.function.Supplier;

public class BackpackContentsMessage {
	private final UUID backpackUuid;
	@Nullable
	private final CompoundNBT backpackContents;

	public BackpackContentsMessage(UUID backpackUuid, @Nullable CompoundNBT backpackContents) {
		this.backpackUuid = backpackUuid;
		this.backpackContents = backpackContents;
	}

	public static void encode(BackpackContentsMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeUUID(msg.backpackUuid);
		packetBuffer.writeNbt(msg.backpackContents);
	}

	public static BackpackContentsMessage decode(PacketBuffer packetBuffer) {
		return new BackpackContentsMessage(packetBuffer.readUUID(), packetBuffer.readNbt());
	}

	static void onMessage(BackpackContentsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(BackpackContentsMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || msg.backpackContents == null) {
			return;
		}

		BackpackStorage.get().setBackpackContents(msg.backpackUuid, msg.backpackContents);
		BackpackTooltipRenderer.refreshContents();
	}
}
