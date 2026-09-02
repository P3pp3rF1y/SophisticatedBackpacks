package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.Optional;

public final class BackpackLinkedStorageResolver {
	private BackpackLinkedStorageResolver() {
	}

	public static Optional<IBackpackWrapper> resolve(Level level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		if (!(level instanceof ServerLevel serverLevel)) {
			return ClientLinkedStorageBackpackContents.getBinding(endpoint.groupId()).map(contents -> {
				ItemStack carrier = stack.copy();
				LinkedStorageStackLifecycle.clear(carrier);
				carrier.removeTagKey(BackpackWrapper.CONTENTS_UUID_TAG);
				ClientLinkedStorageBackpackContents.getStorageSize(endpoint.groupId()).ifPresent(storageSize -> {
					carrier.getOrCreateTag().putInt("inventorySlots", storageSize.inventorySlots());
					carrier.getOrCreateTag().putInt("upgradeSlots", storageSize.upgradeSlots());
				});
				ClientLinkedStorageBackpackContents.getColumnsTaken(endpoint.groupId())
						.ifPresent(columnsTaken -> carrier.getOrCreateTag().putInt("columnsTaken", columnsTaken));
				return new LinkedStorageBackpackWrapper(new BackpackWrapper(stack), new BackpackLinkedStorageHostWrapper(contents, carrier));
			});
		}
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(serverLevel).manager();
		if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
			throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
		}
		ILinkedStorageVirtualHost virtualHost = manager.resolveVirtualHost(endpoint.groupId())
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack host"));
		if (!(virtualHost instanceof IBackpackWrapper host)) {
			throw new IllegalStateException("Linked storage group does not have a backpack host");
		}
		LinkedStorageBackpackWrapper facade = new LinkedStorageBackpackWrapper(new BackpackWrapper(stack), host);
		facade.setGroupChangeSubscription(manager.subscribeToGroupChanges(endpoint.groupId(), facade::onCanonicalContentsChanged));
		return Optional.of(facade);
	}

	public static IBackpackWrapper resolveOrCreate(Level level, ItemStack stack) {
		return resolve(level, stack).orElseGet(() -> new BackpackWrapper(stack));
	}

	public static Optional<IBackpackWrapper> resolveCanonicalHost(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
			return Optional.empty();
		}
		return manager.resolveVirtualHost(endpoint.groupId()).filter(IBackpackWrapper.class::isInstance).map(IBackpackWrapper.class::cast);
	}

	public static Optional<IBackpackWrapper> resolvePrimaryCanonicalHost(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		return LinkedStorageGroupsSavedData.get(level).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())
				? resolveCanonicalHost(level, stack)
				: Optional.empty();
	}

	public static IBackpackWrapper resolveForGlobalUpgradeProcessing(Level level, ItemStack stack) {
		if (level instanceof ServerLevel serverLevel && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
			LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
			LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(serverLevel).manager();
			if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
				throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
			}
			return manager.isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())
					? resolvePrimaryCanonicalHost(serverLevel, stack)
							.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack primary endpoint"))
					: IBackpackWrapper.Noop.INSTANCE;
		}
		return new BackpackWrapper(stack);
	}

	public static boolean synchronizeRenderProjection(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return false;
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		return resolveCanonicalHost(level, stack).map(host -> {
			boolean projectionChanged = false;
			BackpackWrapper physicalBackpack = new BackpackWrapper(stack);
			if (physicalBackpack.getColumnsTaken() != host.getColumnsTaken()) {
				physicalBackpack.setColumnsTaken(host.getColumnsTaken(), false);
				projectionChanged = true;
			}
			long revision = manager.getRenderRevision(endpoint.groupId());
			if (LinkedStorageStackData.getRenderRevision(stack) == revision) {
				return projectionChanged;
			}
			CompoundTag renderInfo = host.getRenderInfo().getNbt();
			CompoundTag tag = stack.getOrCreateTag();
			if (!tag.getCompound("renderInfo").equals(renderInfo)) {
				tag.put("renderInfo", renderInfo.copy());
				projectionChanged = true;
			}
			LinkedStorageStackData.setRenderRevision(stack, revision);
			return projectionChanged;
		}).orElse(false);
	}

	public static boolean hasSameEndpoint(ItemStack first, ItemStack second) {
		LinkedStorageEndpointData firstEndpoint = LinkedStorageStackData.getEndpoint(first);
		LinkedStorageEndpointData secondEndpoint = LinkedStorageStackData.getEndpoint(second);
		return firstEndpoint != null && firstEndpoint.equals(secondEndpoint);
	}
}
