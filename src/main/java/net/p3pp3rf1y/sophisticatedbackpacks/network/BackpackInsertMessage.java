package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.fmllegacy.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackInsertMessage {
	private final int slotIndex;

	public BackpackInsertMessage(int slotIndex) {
		this.slotIndex = slotIndex;
	}

	public static void encode(BackpackInsertMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
	}

	public static BackpackInsertMessage decode(FriendlyByteBuf packetBuffer) {
		return new BackpackInsertMessage(packetBuffer.readInt());
	}

	static void onMessage(BackpackInsertMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, BackpackInsertMessage msg) {
		if (player == null) {
			return;
		}

		AbstractContainerMenu containerMenu = player.containerMenu;
		containerMenu.getSlot(msg.slotIndex).getItem().getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> {
			ItemStack heldItem = containerMenu.getCarried();
			containerMenu.setCarried(InventoryHelper.insertIntoInventory(heldItem, wrapper.getInventoryForUpgradeProcessing(), false));
		});
	}

}
