package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.Container;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackInsertMessage {
	private final int slotIndex;

	public BackpackInsertMessage(int slotIndex) {
		this.slotIndex = slotIndex;
	}

	public static void encode(BackpackInsertMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
	}

	public static BackpackInsertMessage decode(PacketBuffer packetBuffer) {
		return new BackpackInsertMessage(packetBuffer.readInt());
	}

	static void onMessage(BackpackInsertMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, BackpackInsertMessage msg) {
		if (player == null) {
			return;
		}

		Container containerMenu = player.containerMenu;
		containerMenu.getSlot(msg.slotIndex).getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			ItemStack heldItem = player.inventory.getCarried();
			player.inventory.setCarried(InventoryHelper.insertIntoInventory(heldItem, wrapper.getInventoryForUpgradeProcessing(), false));
			player.ignoreSlotUpdateHack = false;
			player.broadcastCarriedItem();

		});
	}
}
