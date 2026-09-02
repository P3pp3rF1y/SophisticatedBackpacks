package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageItemEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

public class BackpackLinkedStorageEndpointAdapter implements ILinkedStorageItemEndpointAdapter {
	@Override
	public boolean supports(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem;
	}

	@Override
	public ResourceLocation factoryId() {
		return BackpackLinkedStorageHostWrapper.FACTORY_ID;
	}

	@Override
	public Compatibility getCompatibility(ServerLevel level, ItemStack stack, LinkedStorageHostDescriptor hostDescriptor) {
		return new BackpackWrapper(stack).getContentsUuid().flatMap(BackpackStorage.get(level)::getBackpackContents)
				.map(BackpackLinkedStorageEndpointAdapter::hasItems).filter(Boolean::booleanValue).map(value -> Compatibility.HAS_CONTENTS)
				.orElse(Compatibility.COMPATIBLE);
	}

	private static boolean hasItems(CompoundTag contents) {
		return hasItems(contents, InventoryHandler.INVENTORY_TAG) || hasItems(contents, UpgradeHandler.UPGRADE_INVENTORY_TAG);
	}

	private static boolean hasItems(CompoundTag contents, String key) {
		return contents.contains(key, Tag.TAG_COMPOUND) && !contents.getCompound(key).getList("Items", Tag.TAG_COMPOUND).isEmpty();
	}

	@Override
	public LinkedStorageHostDescriptor createHostDescriptor(ServerLevel level, ItemStack stack) {
		ItemStack carrier = stack.copy();
		carrier.removeTagKey(BackpackWrapper.CONTENTS_UUID_TAG);
		LinkedStorageStackLifecycle.clear(carrier);
		return new LinkedStorageHostDescriptor(factoryId(), carrier.save(new CompoundTag()));
	}

	@Override
	public CompoundTag copyCanonicalContents(ServerLevel level, ItemStack stack) {
		return new BackpackWrapper(stack).copyContentsForLinkedStorage();
	}

	@Override
	public void bindEndpoint(ServerLevel level, ItemStack stack, LinkedStorageEndpointData endpoint) {
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		ItemStack primaryCarrier = ItemStack.of(manager.getHostDescriptor(endpoint.groupId()).orElseThrow().virtualCarrier());
		new BackpackWrapper(stack).removeContentsUuid();
		copySlotSizes(stack, primaryCarrier);
		LinkedStorageStackData.setEndpoint(stack, endpoint);
		LinkedStorageStackData.setPrimaryEndpoint(stack, manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId()));
		LinkedStorageStackData.setRenderRevision(stack, -1);
	}

	@Override
	public void onEndpointLinked(ServerLevel level, ItemStack endpoint) {
		for (ServerPlayer player : level.getServer().getPlayerList().getPlayers()) {
			if (player.serverLevel() == level && player.containerMenu instanceof IContextAwareContainer container
					&& container.getBackpackContext().getBackpackWrapper(player).getBackpack() == endpoint) {
				player.closeContainer();
			}
		}
	}

	public static boolean completePrimaryTierUpgrade(ServerLevel level, ItemStack result, Container inputs) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(result);
		if (endpoint == null || !(result.getItem() instanceof BackpackItem item)) {
			return false;
		}
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId()) || !hasOriginalEndpoint(inputs, endpoint, result)) {
			return false;
		}
		result.getOrCreateTag().putInt("inventorySlots", item.getNumberOfSlots());
		result.getOrCreateTag().putInt("upgradeSlots", item.getNumberOfUpgradeSlots());
		ItemStack carrier = result.copy();
		carrier.removeTagKey(BackpackWrapper.CONTENTS_UUID_TAG);
		LinkedStorageStackLifecycle.clear(carrier);
		return manager.updatePrimaryHostDescriptor(endpoint.groupId(), endpoint.endpointId(),
				new LinkedStorageHostDescriptor(BackpackLinkedStorageHostWrapper.FACTORY_ID, carrier.save(new CompoundTag())));
	}

	private static boolean hasOriginalEndpoint(Container inputs, LinkedStorageEndpointData endpoint, ItemStack result) {
		for (int slot = 0; slot < inputs.getContainerSize(); slot++) {
			LinkedStorageEndpointData input = LinkedStorageStackData.getEndpoint(inputs.getItem(slot));
			if (input != null && input.groupId().equals(endpoint.groupId()) && input.endpointId().equals(endpoint.endpointId())
					&& inputs.getItem(slot).getItem() != result.getItem()) {
				return true;
			}
		}
		return false;
	}

	private static void copySlotSizes(ItemStack target, ItemStack source) {
		CompoundTag sourceTag = source.getTag();
		if (sourceTag != null && sourceTag.contains("inventorySlots")) {
			target.getOrCreateTag().putInt("inventorySlots", sourceTag.getInt("inventorySlots"));
		}
		if (sourceTag != null && sourceTag.contains("upgradeSlots")) {
			target.getOrCreateTag().putInt("upgradeSlots", sourceTag.getInt("upgradeSlots"));
		}
	}
}
