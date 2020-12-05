package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class ServerUpgradeDataMessage {
	@Nullable
	private final CompoundNBT data;

	public ServerUpgradeDataMessage(@Nullable CompoundNBT data) {
		this.data = data;
	}

	public static void encode(ServerUpgradeDataMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeCompoundTag(msg.data);
	}

	public static ServerUpgradeDataMessage decode(PacketBuffer packetBuffer) {
		return new ServerUpgradeDataMessage(packetBuffer.readCompoundTag());
	}

	public static void onMessage(ServerUpgradeDataMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(contextSupplier.get().getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity sender, ServerUpgradeDataMessage message) {
		if (sender == null || message.data == null) {
			return;
		}

		if (sender.openContainer instanceof BackpackContainer) {
			BackpackContainer backpackContainer = (BackpackContainer) sender.openContainer;
			backpackContainer.handleMessage(message.data);

			sender.openContainer.detectAndSendChanges();
			sender.isChangingQuantityOnly = false;
		}
	}
}
