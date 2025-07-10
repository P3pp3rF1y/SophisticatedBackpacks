package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SophisticatedMenuProvider;

public record BackpackOpenPayload(int slotIndex, String identifier, String handlerName) implements CustomPacketPayload {
	public static final Type<BackpackOpenPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("backpack_open"));
	private static final int CHEST_SLOT = 38;
	private static final int OFFHAND_SLOT = 40;

	public static final StreamCodec<ByteBuf, BackpackOpenPayload> STREAM_CODEC = StreamCodec.composite(
			ByteBufCodecs.INT,
			BackpackOpenPayload::slotIndex,
			ByteBufCodecs.STRING_UTF8,
			BackpackOpenPayload::identifier,
			ByteBufCodecs.STRING_UTF8,
			BackpackOpenPayload::handlerName,
			BackpackOpenPayload::new);

	public BackpackOpenPayload() {
		this(-1);
	}

	public BackpackOpenPayload(int backpackSlot) {
		this(backpackSlot, "");
	}

	public BackpackOpenPayload(int backpackSlot, String identifier) {
		this(backpackSlot, identifier, "");
	}

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(BackpackOpenPayload payload, IPayloadContext context) {
		Player player = context.player();

		if (!payload.handlerName.isEmpty()) {
			int adjustedSlotIndex = payload.slotIndex;
			if (adjustedSlotIndex == CHEST_SLOT) {
				adjustedSlotIndex -= 36;
			} else if (adjustedSlotIndex == OFFHAND_SLOT) {
				adjustedSlotIndex = 0;
			}
			BackpackContext.Item backpackContext = new BackpackContext.Item(payload.handlerName, payload.identifier, adjustedSlotIndex,
					player.containerMenu instanceof InventoryMenu || (player.containerMenu instanceof BackpackContainer backpackContainer && backpackContainer.getBackpackContext().wasOpenFromInventory()));
			openBackpack(player, backpackContext);
		} else if (player.containerMenu instanceof BackpackContainer backpackContainer) {
			BackpackContext backpackContext = backpackContainer.getBackpackContext();
			if (payload.slotIndex == -1) {
				openBackpack(player, backpackContext.getParentBackpackContext());
			} else if (backpackContainer.isStorageInventorySlot(payload.slotIndex)) {
				openBackpack(player, backpackContext.getSubBackpackContext(payload.slotIndex, BackpackWrapper.fromStack(backpackContext.getBackpackWrapper(player).getInventoryHandler().getSlotStack(payload.slotIndex)).getContentsUuid().isEmpty()));
			}
		} else if (player.containerMenu instanceof IContextAwareContainer contextAwareContainer) {
			BackpackContext backpackContext = contextAwareContainer.getBackpackContext();
			openBackpack(player, backpackContext);
		} else {
			findAndOpenFirstBackpack(player);
		}
	}

	private static void findAndOpenFirstBackpack(Player player) {
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, identifier, slot) -> {
			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryName, identifier, slot);
			player.openMenu(new SophisticatedMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName(), false), backpackContext::toBuffer);
			return true;
		});
	}

	private static void openBackpack(Player player, BackpackContext backpackContext) {
		player.openMenu(new SophisticatedMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpackContext.getDisplayName(player), false), backpackContext::toBuffer);
	}
}
