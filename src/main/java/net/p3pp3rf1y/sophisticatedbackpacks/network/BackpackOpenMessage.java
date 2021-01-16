package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.text.StringTextComponent;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackOpenMessage {
	private final int subBackpackSlotIndex;

	public BackpackOpenMessage() {
		this(-1);
	}

	public BackpackOpenMessage(int subBackpackSlotIndex) {
		this.subBackpackSlotIndex = subBackpackSlotIndex;
	}

	public static void encode(BackpackOpenMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.subBackpackSlotIndex);
	}

	public static BackpackOpenMessage decode(PacketBuffer packetBuffer) {
		return new BackpackOpenMessage(packetBuffer.readInt());
	}

	static void onMessage(BackpackOpenMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, BackpackOpenMessage msg) {
		if (player == null) {
			return;
		}

		if (player.openContainer instanceof BackpackContainer) {
			BackpackContainer backpackContainer = (BackpackContainer) player.openContainer;
			if (msg.subBackpackSlotIndex == -1) {
				openParentBackpack(player, backpackContainer);
			} else {
				openSubBackpack(player, msg, backpackContainer);
			}
		} else {
			findAndOpenFirstBackpack(player);
		}
	}

	private static void findAndOpenFirstBackpack(@Nonnull ServerPlayerEntity player) {
		PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryName, slot);
			NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getDisplayName()),
					backpackContext::toBuffer);
			return true;
		});
	}

	private static void openSubBackpack(@Nonnull ServerPlayerEntity player, BackpackOpenMessage msg, BackpackContainer backpackContainer) {
		BackpackContext backpackContext = backpackContainer.getBackpackContext().getSubBackpackContext(msg.subBackpackSlotIndex);
		ItemStack subBackpack = backpackContainer.inventorySlots.get(msg.subBackpackSlotIndex).getStack();
		NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), new StringTextComponent("... > " + subBackpack.getDisplayName().getString())),
				backpackContext::toBuffer);
	}

	private static void openParentBackpack(@Nonnull ServerPlayerEntity player, BackpackContainer backpackContainer) {
		BackpackContext backpackContext = backpackContainer.getBackpackContext().getParentBackpackContext();
		ItemStack parentBackpack = backpackContainer.getParentBackpackWrapper().getBackpack();
		NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), parentBackpack.getDisplayName()),
				backpackContext::toBuffer);
	}
}
