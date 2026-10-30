package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.client.Minecraft;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedcore.network.RequestLinkedStorageContentsMessage;

import java.util.UUID;
import java.util.function.Consumer;

public class BackpackItemClient {
	private BackpackItemClient() {
	}

	public static void sendContentsRequest(ItemStack backpack, UUID uuid) {
		Player player = Minecraft.getInstance().player;
		if (player == null) {
			return;
		}
		Consumer<ItemStackReference> sendRequest = reference -> {
			if (LinkedStorageStackData.getEndpoint(backpack) != null) {
				PacketHandler.INSTANCE
						.sendToServer(new RequestLinkedStorageContentsMessage(uuid, ClientLinkedStorageContents.getRevision(uuid).orElse(-1L), reference));
			} else {
				SBPPacketHandler.INSTANCE.sendToServer(new RequestBackpackInventoryContentsMessage(uuid, reference));
			}
		};
		ItemStackReference.find(player, backpack).ifPresentOrElse(sendRequest,
				() -> PlayerInventoryProvider.get().runOnBackpacks(player, (stack, handlerName, identifier, slot) -> {
					if (!ItemStack.isSameItemSameTags(stack, backpack)) {
						return false;
					}
					sendRequest.accept(new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, slot, handlerName, identifier));
					return true;
				}));
	}
}
