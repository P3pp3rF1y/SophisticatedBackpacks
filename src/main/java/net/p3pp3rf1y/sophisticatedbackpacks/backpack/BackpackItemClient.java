package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.RequestLinkedStorageContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.network.ItemStackReference;

import javax.annotation.Nullable;

import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

public class BackpackItemClient {
	@Nullable
	public static TooltipComponent getTooltipImage(ItemStack stack) {
		Minecraft mc = Minecraft.getInstance();
		Optional<LinkedStorageEndpointRole> linkedStorageRole = BackpackItem.getLinkedStorageEndpointRole(stack);
		if (linkedStorageRole.isPresent() && !Screen.hasShiftDown() && (mc.player == null || mc.player.containerMenu.getCarried().isEmpty())) {
			LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			if (mc.player != null && ClientLinkedStorageContents.shouldRequestSnapshot(endpoint.groupId(), mc.player.level().getGameTime())) {
				sendContentsRequest(stack, endpoint.groupId());
			}
			return new BackpackItem.LinkedStorageTooltip(linkedStorageRole.get(), endpoint.groupId());
		}
		if (Screen.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
			return new BackpackItem.BackpackContentsTooltip(stack);
		}
		return null;
	}

	public static void sendContentsRequest(ItemStack backpack, UUID uuid) {
		Player player = Minecraft.getInstance().player;
		if (player == null) {
			return;
		}
		Consumer<ItemStackReference> sendRequest = reference -> {
			if (backpack.has(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT)) {
				PacketDistributor
						.sendToServer(new RequestLinkedStorageContentsPayload(uuid, ClientLinkedStorageContents.getRevision(uuid).orElse(-1L), reference));
			} else {
				PacketDistributor.sendToServer(new RequestBackpackInventoryContentsPayload(uuid, reference));
			}
		};
		ItemStackReference.find(player, backpack).ifPresentOrElse(sendRequest,
				() -> PlayerInventoryProvider.get().runOnBackpacks(player, (stack, handlerName, identifier, slot) -> {
					if (!ItemStack.isSameItemSameComponents(stack, backpack)) {
						return false;
					}
					sendRequest.accept(new ItemStackReference(ItemStackReference.EQUIPMENT_HANDLER, slot, handlerName, identifier));
					return true;
				}));
	}
}
