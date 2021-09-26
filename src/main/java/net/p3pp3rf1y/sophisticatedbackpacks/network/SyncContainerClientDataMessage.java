package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fmllegacy.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ISyncedContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class SyncContainerClientDataMessage {
	@Nullable
	private final CompoundTag data;

	public SyncContainerClientDataMessage(@Nullable CompoundTag data) {
		this.data = data;
	}

	public static void encode(SyncContainerClientDataMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeNbt(msg.data);
	}

	public static SyncContainerClientDataMessage decode(FriendlyByteBuf packetBuffer) {
		return new SyncContainerClientDataMessage(packetBuffer.readNbt());
	}

	public static void onMessage(SyncContainerClientDataMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(contextSupplier.get().getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer sender, SyncContainerClientDataMessage message) {
		if (sender == null || message.data == null) {
			return;
		}

		if (sender.containerMenu instanceof ISyncedContainer container) {
			container.handleMessage(message.data);
		}
	}
}
