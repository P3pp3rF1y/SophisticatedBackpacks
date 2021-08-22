package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class TransferFullSlotMessage {
	private final int slotId;

	public TransferFullSlotMessage(int slotId) {
		this.slotId = slotId;
	}

	public static void encode(TransferFullSlotMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.slotId);
	}

	public static TransferFullSlotMessage decode(PacketBuffer packetBuffer) {
		return new TransferFullSlotMessage(packetBuffer.readInt());
	}

	static void onMessage(TransferFullSlotMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, TransferFullSlotMessage msg) {
		if (player == null || !(player.containerMenu instanceof BackpackContainer)) {
			return;
		}
		BackpackContainer backpackContainer = (BackpackContainer) player.containerMenu;
		Slot slot = backpackContainer.getSlot(msg.slotId);
		ItemStack transferResult;
		do {
			transferResult = backpackContainer.quickMoveStack(player, msg.slotId);
		} while (!transferResult.isEmpty() && ItemStack.isSame(slot.getItem(), transferResult));
	}
}
