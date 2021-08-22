package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ISyncedContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class SyncContainerClientDataMessage {
	@Nullable
	private final CompoundNBT data;

	public SyncContainerClientDataMessage(@Nullable CompoundNBT data) {
		this.data = data;
	}

	public static void encode(SyncContainerClientDataMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeNbt(msg.data);
	}

	public static SyncContainerClientDataMessage decode(PacketBuffer packetBuffer) {
		return new SyncContainerClientDataMessage(packetBuffer.readNbt());
	}

	public static void onMessage(SyncContainerClientDataMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(contextSupplier.get().getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity sender, SyncContainerClientDataMessage message) {
		if (sender == null || message.data == null) {
			return;
		}

		if (sender.containerMenu instanceof ISyncedContainer) {
			ISyncedContainer container = (ISyncedContainer) sender.containerMenu;
			container.handleMessage(message.data);
		}
	}
}
