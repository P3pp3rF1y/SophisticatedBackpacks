package net.p3pp3rf1y.sophisticatedcore.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;

import java.util.function.Supplier;

public class SyncSlotStackMessage {
	private final int windowId;
	private final int stateId;
	private final int slotNumber;
	private final ItemStack stack;

	public SyncSlotStackMessage(int windowId, int stateId, int slotNumber, ItemStack stack) {
		this.windowId = windowId;
		this.stateId = stateId;
		this.slotNumber = slotNumber;
		this.stack = stack;
	}

	public static void encode(SyncSlotStackMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeByte(msg.windowId);
		packetBuffer.writeVarInt(msg.stateId);
		packetBuffer.writeShort(msg.slotNumber);
		PacketHelper.writeItemStack(msg.stack, packetBuffer);
	}

	public static SyncSlotStackMessage decode(FriendlyByteBuf packetBuffer) {
		return new SyncSlotStackMessage(packetBuffer.readUnsignedByte(), packetBuffer.readVarInt(), packetBuffer.readShort(), PacketHelper.readItemStack(packetBuffer));
	}

	public static void onMessage(SyncSlotStackMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncSlotStackMessage msg) {
		LocalPlayer player = Minecraft.getInstance().player;
		if (player == null || !(player.containerMenu instanceof StorageContainerMenuBase || player.containerMenu instanceof SettingsContainer) || player.containerMenu.containerId != msg.windowId) {
			return;
		}
		player.containerMenu.setItem(msg.slotNumber, msg.stateId, msg.stack);
	}
}
