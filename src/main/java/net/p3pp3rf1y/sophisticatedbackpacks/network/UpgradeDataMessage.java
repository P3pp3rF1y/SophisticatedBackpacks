package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class UpgradeDataMessage {
	private final CompoundNBT data;

	public UpgradeDataMessage(CompoundNBT data) {
		this.data = data;
	}

	public static void encode(UpgradeDataMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeCompoundTag(msg.data);
	}

	public static UpgradeDataMessage decode(PacketBuffer packetBuffer) {
		return new UpgradeDataMessage(packetBuffer.readCompoundTag());
	}

	public static void onMessage(UpgradeDataMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(contextSupplier.get().getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity sender, UpgradeDataMessage message) {
		if (sender == null) {
			return;
		}

		if (sender.openContainer instanceof BackpackContainer) {
			((BackpackContainer) sender.openContainer).handleMessage(message.data);
		}
		sender.isChangingQuantityOnly = true; //prevents sending SSetSlotPacket on upgrade nbt change, updating of both client and server is taken care of by container
		sender.openContainer.detectAndSendChanges();
		sender.isChangingQuantityOnly = false;
	}
}
