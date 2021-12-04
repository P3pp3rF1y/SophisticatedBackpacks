package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class TransferMessage {
	private final boolean isRestock;

	public TransferMessage(boolean isRestock) {
		this.isRestock = isRestock;
	}

	public static void encode(TransferMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeBoolean(msg.isRestock);
	}

	public static TransferMessage decode(PacketBuffer packetBuffer) {
		return new TransferMessage(packetBuffer.readBoolean());
	}

	static void onMessage(TransferMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, TransferMessage msg) {
		if (player == null || !(player.containerMenu instanceof BackpackContainer)) {
			return;
		}
		BackpackContainer backpackContainer = (BackpackContainer) player.containerMenu;
		BackpackInventoryHandler backpackInventory = backpackContainer.getBackpackWrapper().getInventoryHandler();
		if (msg.isRestock) {
			player.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).ifPresent(playerInv -> {
				InventoryHelper.transfer(backpackInventory, playerInv, s -> {});
			});
		} else {
			PlayerInventory inv = player.inventory;
			for(int i = PlayerInventory.getSelectionSize(); i < inv.items.size(); i++) {
				ItemStack stackAt = inv.getItem(i);
				if(!stackAt.isEmpty()) {
					inv.setItem(i, backpackInventory.insertItem(stackAt, false));
				}
			}
		}
	}
}
