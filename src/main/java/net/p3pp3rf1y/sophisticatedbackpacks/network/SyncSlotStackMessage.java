package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;

import java.util.function.Supplier;

public class SyncSlotStackMessage {
	private final int windowId;
	private final int slotNumber;
	private final ItemStack stack;

	public SyncSlotStackMessage(int windowId, int slotNumber, ItemStack stack) {
		this.windowId = windowId;
		this.slotNumber = slotNumber;
		this.stack = stack;
	}

	public static void encode(SyncSlotStackMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeByte(msg.windowId);
		packetBuffer.writeShort(msg.slotNumber);
		PacketHelper.writeItemStack(msg.stack, packetBuffer);
	}

	public static SyncSlotStackMessage decode(PacketBuffer packetBuffer) {
		return new SyncSlotStackMessage(packetBuffer.readUnsignedByte(), packetBuffer.readShort(), PacketHelper.readItemStack(packetBuffer));
	}

	static void onMessage(SyncSlotStackMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncSlotStackMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || !(player.containerMenu instanceof BackpackContainer || player.containerMenu instanceof SettingsContainer) || player.containerMenu.containerId != msg.windowId) {
			return;
		}
		player.containerMenu.setItem(msg.slotNumber, msg.stack);
	}
}
