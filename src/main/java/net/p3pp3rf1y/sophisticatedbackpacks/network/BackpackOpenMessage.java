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
	private final String identifier;
	private final String handlerName;
	public BackpackOpenMessage() {
		this(-1);
	}

	public BackpackOpenMessage(int backpackSlot) {
		this(backpackSlot, "");
	}

	public BackpackOpenMessage(int backpackSlot, String identifier, String handlerName) {
		slotIndex = backpackSlot;
		this.identifier = identifier;
		this.handlerName = handlerName;
	}

	public BackpackOpenMessage(int backpackSlot, String identifier) {
		this(backpackSlot, identifier, "");
	}

	public static void encode(BackpackOpenMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
		packetBuffer.writeUtf(msg.identifier);
		packetBuffer.writeUtf(msg.handlerName);
	}

	public static BackpackOpenMessage decode(FriendlyByteBuf packetBuffer) {
		return new BackpackOpenMessage(packetBuffer.readInt(), packetBuffer.readUtf(), packetBuffer.readUtf());
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

		if (!msg.handlerName.isEmpty()) {
			int slotIndex = msg.slotIndex;
			if (msg.slotIndex == CHEST_SLOT) {
				slotIndex -= 36;
			} else if (msg.slotIndex == OFFHAND_SLOT) {
				slotIndex = 0;
			}
			BackpackContext.Item backpackContext = new BackpackContext.Item(msg.handlerName, msg.identifier, slotIndex,
					player.containerMenu instanceof InventoryMenu || (player.containerMenu instanceof BackpackContainer backpackContainer && backpackContainer.getBackpackContext().wasOpenFromInventory()));
			openBackpack(player, backpackContext);
		} else if (player.containerMenu instanceof BackpackContainer backpackContainer) {
			BackpackContext backpackContext = backpackContainer.getBackpackContext();
			if (msg.slotIndex == -1) {
				openBackpack(player, backpackContext.getParentBackpackContext());
			} else if (backpackContainer.isStorageInventorySlot(msg.slotIndex)) {
				openBackpack(player, backpackContext.getSubBackpackContext(msg.slotIndex));
			}
		} else if (player.containerMenu instanceof IContextAwareContainer contextAwareContainer) {
			BackpackContext backpackContext = contextAwareContainer.getBackpackContext();
			openBackpack(player, backpackContext);
		} else {
			findAndOpenFirstBackpack(player);
		}
	}

	private static void findAndOpenFirstBackpack(ServerPlayer player) {
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, identifier, slot) -> {
			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryName, identifier, slot);
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
