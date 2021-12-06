package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class SetGhostSlotMessage {
	private final ItemStack stack;
	private final int slotNumber;

	public SetGhostSlotMessage(ItemStack stack, int slotNumber) {
		this.stack = stack;
		this.slotNumber = slotNumber;
	}

	public static void encode(SetGhostSlotMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeItem(msg.stack);
		packetBuffer.writeShort(msg.slotNumber);
	}

	public static SetGhostSlotMessage decode(FriendlyByteBuf packetBuffer) {
		return new SetGhostSlotMessage(packetBuffer.readItem(), packetBuffer.readShort());
	}

	static void onMessage(SetGhostSlotMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg, context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SetGhostSlotMessage msg, @Nullable ServerPlayer sender) {
		if (sender == null || !(sender.containerMenu instanceof BackpackContainer)) {
			return;
		}
		sender.containerMenu.getSlot(msg.slotNumber).set(msg.stack);
	}
}
