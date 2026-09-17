package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContents;
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
			Optional<ILinkedStorageContents> contents = ClientLinkedStorageContents.getContents(endpoint.groupId());
			if (contents.isEmpty()) {
				return Optional.empty();
			}
			ItemStack carrier = stack.copy();
			LinkedStorageStackLifecycle.clear(carrier);
			carrier.removeTagKey(BackpackWrapper.CONTENTS_UUID_TAG);
			ClientLinkedStorageContents.getInventorySlots(endpoint.groupId())
					.ifPresent(inventorySlots -> carrier.getOrCreateTag().putInt("inventorySlots", inventorySlots));
			ClientLinkedStorageContents.getUpgradeSlots(endpoint.groupId())
					.ifPresent(upgradeSlots -> carrier.getOrCreateTag().putInt("upgradeSlots", upgradeSlots));
			ClientLinkedStorageContents.getColumnsTaken(endpoint.groupId())
					.ifPresent(columnsTaken -> carrier.getOrCreateTag().putInt("columnsTaken", columnsTaken));
			return Optional.of(new LinkedStorageBackpackWrapper(new BackpackWrapper(stack), new BackpackLinkedStorageHostWrapper(contents.get(), carrier)));
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
		return resolve(level, stack).orElseGet(() -> {
			if (!level.isClientSide && LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
				throw new IllegalStateException("Failed to resolve linked backpack endpoint");
			}
			return new BackpackWrapper(stack);
		});
	}

	public static Optional<IBackpackWrapper> resolveCanonicalHost(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		if (!manager.isEndpointMember(endpoint.groupId(), endpoint.endpointId())) {
			throw new IllegalStateException("Linked backpack endpoint is not registered in its group");
		}
		ILinkedStorageVirtualHost virtualHost = manager.resolveVirtualHost(endpoint.groupId())
				.orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack host for group " + endpoint.groupId()));
		if (!(virtualHost instanceof IBackpackWrapper backpackHost)) {
			throw new IllegalStateException("Linked storage group " + endpoint.groupId() + " does not have a backpack host");
		}
		return Optional.of(backpackHost);
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
		return stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElseGet(() -> new BackpackWrapper(stack));
	}

	public static boolean synchronizeRenderProjection(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return false;
		}
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(stack);
		LinkedStorageGroupManager manager = LinkedStorageGroupsSavedData.get(level).manager();
		long renderRevision = manager.getRenderRevision(endpoint.groupId());
		if (LinkedStorageStackData.getRenderRevision(stack) == renderRevision) {
			return false;
		}
		return manager.resolveVirtualHost(endpoint, false).filter(IBackpackWrapper.class::isInstance).map(IBackpackWrapper.class::cast).map(host -> {
			boolean projectionChanged = false;
			BackpackWrapper physicalBackpack = new BackpackWrapper(stack);
			if (physicalBackpack.getColumnsTaken() != host.getColumnsTaken()) {
				physicalBackpack.setColumnsTaken(host.getColumnsTaken(), false);
				projectionChanged = true;
			}
			CompoundTag renderInfo = host.getRenderInfo().getNbt();
			CompoundTag tag = stack.getOrCreateTag();
			if (!tag.getCompound("renderInfo").equals(renderInfo)) {
				tag.put("renderInfo", renderInfo.copy());
				projectionChanged = true;
			}
			LinkedStorageStackData.setRenderRevision(stack, renderRevision);
			return projectionChanged;
		}).orElseThrow(() -> new IllegalStateException("Failed to resolve linked backpack render host"));
	}

	public static boolean hasSameEndpoint(ItemStack first, ItemStack second) {
		LinkedStorageEndpointData firstEndpoint = LinkedStorageStackData.getEndpoint(first);
		LinkedStorageEndpointData secondEndpoint = LinkedStorageStackData.getEndpoint(second);
		return firstEndpoint != null && firstEndpoint.equals(secondEndpoint);
	}
}
