package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.level.Level;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.Optional;

public final class BackpackLinkedStorageResolver {
	private BackpackLinkedStorageResolver() {
	}

	public static Optional<IBackpackWrapper> resolve(Level level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		if (!(level instanceof ServerLevel serverLevel)) {
			Optional<ILinkedStorageContentsBinding> contents = ClientLinkedStorageBackpackContents.getBinding(endpoint.groupId());
			if (contents.isEmpty()) {
				return Optional.empty();
			}
			ItemStack virtualCarrier = stack.copy();
			LinkedStorageStackLifecycle.clear(virtualCarrier);
			virtualCarrier.remove(ModCoreDataComponents.STORAGE_UUID);
			ClientLinkedStorageBackpackContents.getStorageSize(endpoint.groupId()).ifPresent(storageSize -> {
				virtualCarrier.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, storageSize.inventorySlots());
				virtualCarrier.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, storageSize.upgradeSlots());
			});
			ClientLinkedStorageBackpackContents.getColumnsTaken(endpoint.groupId())
					.ifPresent(columnsTaken -> virtualCarrier.set(ModDataComponents.COLUMNS_TAKEN, columnsTaken));
			return Optional
					.of(new LinkedStorageBackpackWrapper(new BackpackWrapper(stack), new BackpackLinkedStorageHostWrapper(contents.get(), virtualCarrier)));
		}
		return resolveCanonicalHost(serverLevel, stack, false, true).map(host -> {
			LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(serverLevel).manager();
			LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(stack), host);
			facade.setGroupChangeSubscription(manager.subscribeToGroupChanges(endpoint.groupId(), facade::onCanonicalContentsChanged));
			return facade;
		});
	}

	public static IBackpackWrapper resolveOrCreate(Level level, ItemStack stack) {
		return resolve(level, stack).orElseGet(() -> {
			if (!level.isClientSide && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
				throw new IllegalStateException("Failed to resolve linked backpack endpoint");
			}
			return BackpackWrapper.fromStack(stack);
		});
	}

	public static Optional<IBackpackWrapper> resolveServerCanonicalHost(ItemStack stack) {
		if (Thread.currentThread().getThreadGroup() != SidedThreadGroups.SERVER) {
			return Optional.empty();
		}
		MinecraftServer server = ServerLifecycleHooks.getCurrentServer();
		if (server == null) {
			return Optional.empty();
		}
		ServerLevel overworld = server.getLevel(Level.OVERWORLD);
		return overworld == null ? Optional.empty() : resolveCanonicalHost(overworld, stack, false, false);
	}

	public static Optional<IBackpackWrapper> resolveCanonicalHost(ServerLevel level, ItemStack stack) {
		return resolveCanonicalHost(level, stack, false, false);
	}

	public static Optional<IBackpackWrapper> resolvePrimaryCanonicalHost(ServerLevel level, ItemStack stack) {
		return resolveCanonicalHost(level, stack, true, false);
	}

	public static IBackpackWrapper resolveForGlobalUpgradeProcessing(Level level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT && level instanceof ServerLevel serverLevel) {
			LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(serverLevel).manager();
			if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
				throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
			}
			if (!manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())) {
				return IBackpackWrapper.Noop.INSTANCE;
			}
			return resolvePrimaryCanonicalHost(serverLevel, stack)
					.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack primary endpoint"));
		}
		return BackpackWrapper.fromStack(stack);
	}

	public static boolean synchronizeRenderProjection(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return false;
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		long renderRevision = manager.getRenderRevision(endpoint.groupId());
		if (stack.getOrDefault(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION, -1L) == renderRevision) {
			return false;
		}
		return manager.resolveVirtualHost(endpoint, false).filter(IBackpackWrapper.class::isInstance).map(IBackpackWrapper.class::cast).map(host -> {
			CompoundTag canonicalRenderInfo = host.getRenderInfo().getNbt();
			stack.set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(canonicalRenderInfo));
			stack.set(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION, renderRevision);
			return true;
		}).orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack render host"));
	}

	private static Optional<IBackpackWrapper> resolveCanonicalHost(ServerLevel level, ItemStack stack, boolean requirePrimary,
			boolean synchronizePhysicalEndpoint) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
			throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
		}
		if (requirePrimary && !manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())) {
			return Optional.empty();
		}
		ILinkedStorageVirtualHost virtualHost = manager.resolveVirtualHost(endpoint.groupId())
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack host for group " + endpoint.groupId()));
		if (!(virtualHost instanceof IBackpackWrapper backpackHost)) {
			throw new IllegalStateException("Linked storage group " + endpoint.groupId() + " does not have a backpack host");
		}
		if (synchronizePhysicalEndpoint) {
			BackpackLinkedStorageEndpointAdapter.synchronizeEndpointProfile(level, stack);
			BackpackLinkedStorageEndpointAdapter.synchronizePrimaryCarrier(level, stack);
		}
		return Optional.of(backpackHost);
	}
}
