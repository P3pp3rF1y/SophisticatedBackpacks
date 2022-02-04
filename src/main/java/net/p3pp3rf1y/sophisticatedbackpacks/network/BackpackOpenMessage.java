package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackOpenMessage {
	private static final int CHEST_SLOT = 38;
	private static final int OFFHAND_SLOT = 40;
	private final int slotIndex;
	private final boolean isSubBackpack;

	public BackpackOpenMessage() {
		this(-1, false);
	}

	public BackpackOpenMessage(int backpackSlot, boolean isSubBackpack) {
		slotIndex = backpackSlot;
		this.isSubBackpack = isSubBackpack;
	}

	public static void encode(BackpackOpenMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
		packetBuffer.writeBoolean(msg.isSubBackpack);
	}

	public static BackpackOpenMessage decode(FriendlyByteBuf packetBuffer) {
		return new BackpackOpenMessage(packetBuffer.readInt(), packetBuffer.readBoolean());
	}

	static void onMessage(BackpackOpenMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, BackpackOpenMessage msg) {
		if (player == null) {
			return;
		}

		if (player.containerMenu instanceof BackpackContainer backpackContainer) {
			BackpackContext backpackContext = backpackContainer.getBackpackContext();
			if (msg.slotIndex == -1) {
				openBackpack(player, backpackContext.getParentBackpackContext());
			} else if (backpackContainer.isStorageInventorySlot(msg.slotIndex)) {
				openBackpack(player, backpackContext.getSubBackpackContext(msg.slotIndex));
			}
		} else if (player.containerMenu instanceof IContextAwareContainer contextAwareContainer) {
			BackpackContext backpackContext = contextAwareContainer.getBackpackContext();
			openBackpack(player, backpackContext);
		} else if (msg.slotIndex > -1 && player.containerMenu instanceof InventoryMenu) {
			int slotIndex = msg.slotIndex;
			String inventoryProvider = PlayerInventoryProvider.MAIN_INVENTORY;
			if (msg.slotIndex == CHEST_SLOT) {
				inventoryProvider = PlayerInventoryProvider.ARMOR_INVENTORY;
			} else if (msg.slotIndex == OFFHAND_SLOT) {
				inventoryProvider = PlayerInventoryProvider.OFFHAND_INVENTORY;
				slotIndex = 0;
			}

			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryProvider, slotIndex, true);
			openBackpack(player, backpackContext);
		} else {
			findAndOpenFirstBackpack(player);
		}
	}

	private static void findAndOpenFirstBackpack(ServerPlayer player) {
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryName, slot);
			NetworkHooks.openGui(player, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName()),
					backpackContext::toBuffer);
			return true;
		});
	}

	private static void openBackpack(ServerPlayer player, BackpackContext backpackContext) {
		NetworkHooks.openGui(player, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpackContext.getDisplayName(player)),
				backpackContext::toBuffer);
	}
}
