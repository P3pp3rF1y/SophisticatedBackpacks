package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.client.Minecraft;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.RequestLinkedStorageContentsPayload;
import org.jspecify.annotations.Nullable;

import java.util.Optional;

public class BackpackItemClient {
	@Nullable
	public static TooltipComponent getTooltipImage(ItemStack stack) {
		Minecraft mc = Minecraft.getInstance();
		Optional<LinkedStorageEndpointRole> linkedStorageRole = BackpackItem.getLinkedStorageEndpointRole(stack);
		if (linkedStorageRole.isPresent() && !mc.hasShiftDown() && (mc.player == null || mc.player.containerMenu.getCarried().isEmpty())) {
			LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			if (mc.player != null && ClientLinkedStorageContents.shouldRequestSnapshot(endpoint.groupId(), mc.player.level().getGameTime())) {
				ClientPacketDistributor.sendToServer(
						new RequestLinkedStorageContentsPayload(endpoint.groupId(), ClientLinkedStorageContents.getRevision(endpoint.groupId()).orElse(-1L)));
			}
			return new BackpackItem.LinkedStorageTooltip(linkedStorageRole.get(), endpoint.groupId());
		}
		if (mc.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
			return new BackpackItem.BackpackContentsTooltip(stack);
		}
		return null;
	}
}
