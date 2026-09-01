package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageItemEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import java.util.UUID;

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
	public boolean isCompatible(ServerLevel level, ItemStack endpoint, LinkedStorageHostDescriptor hostDescriptor) {
		return getCompatibility(level, endpoint, hostDescriptor) == Compatibility.COMPATIBLE;
	}

	@Override
	public Compatibility getCompatibility(ServerLevel level, ItemStack endpoint, LinkedStorageHostDescriptor hostDescriptor) {
		UUID storageId = endpoint.get(ModCoreDataComponents.STORAGE_UUID);
		return storageId != null && BackpackStorage.get(level).getBackpackContents(storageId)
				.map(BackpackLinkedStorageEndpointAdapter::hasIncompatibleBackpackData).orElse(false) ? Compatibility.HAS_CONTENTS : Compatibility.COMPATIBLE;
	}

	private static boolean hasIncompatibleBackpackData(CompoundTag contents) {
		return hasItems(contents, InventoryHandler.INVENTORY_TAG) || hasItems(contents, UpgradeHandler.UPGRADE_INVENTORY_TAG);
	}

	private static boolean hasItems(CompoundTag contents, String inventoryTag) {
		return contents.contains(inventoryTag) && contents.getCompound(inventoryTag).contains("Items", Tag.TAG_LIST)
				&& !contents.getCompound(inventoryTag).getList("Items", Tag.TAG_COMPOUND).isEmpty();
	}

	@Override
	public LinkedStorageHostDescriptor createHostDescriptor(ServerLevel level, ItemStack stack) {
		ItemStack virtualCarrier = stack.copy();
		virtualCarrier.remove(ModCoreDataComponents.STORAGE_UUID);
		LinkedStorageStackLifecycle.clear(virtualCarrier);
		copySlotSizes(virtualCarrier, virtualCarrier);
		return new LinkedStorageHostDescriptor(factoryId(), (CompoundTag) virtualCarrier.save(level.registryAccess(), new CompoundTag()));
	}

	@Override
	public CompoundTag copyCanonicalContents(ServerLevel level, ItemStack stack) {
		return new BackpackWrapper(stack).copyContentsForLinkedStorage();
	}

	@Override
	public void bindEndpoint(ServerLevel level, ItemStack stack, LinkedStorageEndpointData endpoint) {
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		ItemStack primaryCarrier = manager.getHostDescriptor(endpoint.groupId())
				.map(hostDescriptor -> ItemStack.parseOptional(level.registryAccess(), hostDescriptor.virtualCarrier())).orElseThrow();
		boolean primaryEndpoint = manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId());
		// Resolve all canonical state before detaching the ordinary backend so a failed preparation preserves the source Backpack.
		BackpackWrapper wrapper = new BackpackWrapper(stack);
		wrapper.removeContentsUuid();
		if (primaryEndpoint) {
			manager.resolveContents(endpoint.groupId()).orElseThrow().setColumnsTaken(stack.getOrDefault(ModDataComponents.COLUMNS_TAKEN, 0));
		}
		copySlotSizes(stack, primaryCarrier);
		stack.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, primaryEndpoint);
		stack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, endpoint);
		stack.remove(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION);
	}

	@Override
	public void onEndpointLinked(ServerLevel level, ItemStack endpoint) {
		for (ServerPlayer player : level.getServer().getPlayerList().getPlayers()) {
			if (player.serverLevel() == level && player.containerMenu instanceof IContextAwareContainer contextAwareContainer
					&& contextAwareContainer.getBackpackContext().getBackpackWrapper(player).getBackpack() == endpoint) {
				player.closeContainer();
			}
		}
	}

	public static void synchronizeEndpointProfile(ServerLevel level, ItemStack stack) {
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		LinkedStorageHostDescriptor hostDescriptor = manager.getHostDescriptor(endpoint.groupId()).orElseThrow();
		copySlotSizes(stack, ItemStack.parseOptional(level.registryAccess(), hostDescriptor.virtualCarrier()));
	}

	public static boolean completePrimaryTierUpgrade(ServerLevel level, ItemStack result, Container inputs) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(result) != LinkedStorageEndpointStackState.ENDPOINT
				|| !(result.getItem() instanceof BackpackItem backpackItem)) {
			return false;
		}
		LinkedStorageEndpointData endpoint = result.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId()) || !hasOriginalEndpoint(inputs, endpoint, result)) {
			return false;
		}

		LinkedStorageHostDescriptor hostDescriptor = manager.getHostDescriptor(endpoint.groupId()).orElseThrow();
		setSlotSizes(result, backpackItem);
		result.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		ItemStack existingCarrier = ItemStack.parseOptional(level.registryAccess(), hostDescriptor.virtualCarrier());
		ItemStack upgradedCarrier = result.copyWithCount(1);
		upgradedCarrier.remove(ModCoreDataComponents.STORAGE_UUID);
		LinkedStorageStackLifecycle.clear(upgradedCarrier);
		CustomData renderInfo = existingCarrier.get(ModCoreDataComponents.RENDER_INFO_TAG);
		if (renderInfo != null) {
			upgradedCarrier.set(ModCoreDataComponents.RENDER_INFO_TAG, renderInfo);
		}
		manager.updatePrimaryHostDescriptor(endpoint.groupId(), endpoint.endpointId(),
				new LinkedStorageHostDescriptor(hostDescriptor.factoryId(), (CompoundTag) upgradedCarrier.save(level.registryAccess(), new CompoundTag())));
		return true;
	}

	public static void synchronizePrimaryCarrier(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return;
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())) {
			return;
		}
		LinkedStorageHostDescriptor hostDescriptor = manager.getHostDescriptor(endpoint.groupId()).orElseThrow();
		ItemStack existingCarrier = ItemStack.parseOptional(level.registryAccess(), hostDescriptor.virtualCarrier());
		if (existingCarrier.is(stack.getItem()) && existingCarrier.getHoverName().equals(stack.getHoverName())) {
			return;
		}
		ItemStack updatedCarrier = new ItemStack(stack.getItem());
		updatedCarrier.applyComponents(existingCarrier.getComponents());
		updatedCarrier.set(DataComponents.CUSTOM_NAME, stack.getHoverName());
		manager.updatePrimaryHostDescriptor(endpoint.groupId(), endpoint.endpointId(),
				new LinkedStorageHostDescriptor(hostDescriptor.factoryId(), (CompoundTag) updatedCarrier.save(level.registryAccess(), new CompoundTag())));
	}

	private static boolean hasOriginalEndpoint(Container inputs, LinkedStorageEndpointData endpoint, ItemStack result) {
		for (int slot = 0; slot < inputs.getContainerSize(); slot++) {
			ItemStack input = inputs.getItem(slot);
			LinkedStorageEndpointData inputEndpoint = input.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			if (inputEndpoint != null && endpoint.groupId().equals(inputEndpoint.groupId()) && endpoint.endpointId().equals(inputEndpoint.endpointId())
					&& input.getItem() != result.getItem()) {
				return true;
			}
		}
		return false;
	}

	private static void copySlotSizes(ItemStack target, ItemStack source) {
		BackpackItem backpackItem = (BackpackItem) source.getItem();
		int inventorySlots = source.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots());
		int upgradeSlots = source.getOrDefault(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots());
		if (target.getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots()) != inventorySlots) {
			target.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, inventorySlots);
		}
		if (target.getOrDefault(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots()) != upgradeSlots) {
			target.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, upgradeSlots);
		}
	}

	private static void setSlotSizes(ItemStack stack, BackpackItem backpackItem) {
		stack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots());
		stack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots());
	}
}
