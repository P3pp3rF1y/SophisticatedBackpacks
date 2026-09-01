package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.client.Minecraft;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestLinkedStorageBackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;

import javax.annotation.Nullable;

import java.util.Optional;

public class BackpackItemClient {
	@Nullable
	public static TooltipComponent getTooltipImage(ItemStack stack) {
		Minecraft mc = Minecraft.getInstance();
		Optional<LinkedStorageEndpointRole> linkedStorageRole = BackpackItem.getLinkedStorageEndpointRole(stack);
		if (linkedStorageRole.isPresent() && !mc.hasShiftDown() && (mc.player == null || mc.player.containerMenu.getCarried().isEmpty())) {
			LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			if (ClientLinkedStorageBackpackContents.getGroupName(endpoint.groupId()).isEmpty()
					&& ClientLinkedStorageBackpackContents.requestGroupName(endpoint.groupId())) {
				ClientPacketDistributor.sendToServer(new RequestLinkedStorageBackpackContentsPayload(endpoint.groupId(), -1L));
			}
			return new BackpackItem.LinkedStorageTooltip(linkedStorageRole.get(), endpoint.groupId());
		}
		if (mc.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
			return new BackpackItem.BackpackContentsTooltip(stack);
		}
		return null;
	}
}
