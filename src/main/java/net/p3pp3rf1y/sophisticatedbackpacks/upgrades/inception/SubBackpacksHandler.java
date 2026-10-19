package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.common.util.LazyOptional;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.IntConsumer;

public class SubBackpacksHandler {
	private final Map<Integer, IStorageWrapper> subBackpacks = new LinkedHashMap<>();

	private final InventoryHandler inventoryHandler;
	private final IntConsumer contentsChangedListener = this::onContentsChanged;
	private final Set<Consumer<Collection<IStorageWrapper>>> refreshListeners = new HashSet<>();
	private final Set<Consumer<Collection<IStorageWrapper>>> beforeRefreshListeners = new HashSet<>();
	private final Set<Runnable> upgradeCachesInvalidatedListeners = new HashSet<>();

	public SubBackpacksHandler(InventoryHandler inventoryHandler) {
		this.inventoryHandler = inventoryHandler;
		ensureInventoryListenerRegistered();
	}

	public void ensureInventoryListenerRegistered() {
		inventoryHandler.removeListener(contentsChangedListener);
		inventoryHandler.addListener(contentsChangedListener);
		refreshSubBackpacksIfChanged();
	}

	private void refreshSubBackpacksIfChanged() {
		Map<Integer, IStorageWrapper> refreshed = getSubBackpacksFromInventory();
		if (!subBackpacks.equals(refreshed)) {
			notifyBeforeRefresh();
			subBackpacks.clear();
			subBackpacks.putAll(refreshed);
			notifyAfterRefresh();
		}
	}

	public void addRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		refreshListeners.add(listener);
	}

	public void removeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		refreshListeners.remove(listener);
	}

	public void addUpgradeCachesInvalidatedListener(Runnable listener) {
		upgradeCachesInvalidatedListeners.add(listener);
	}

	public void removeUpgradeCachesInvalidatedListener(Runnable listener) {
		upgradeCachesInvalidatedListeners.remove(listener);
	}

	public void onUpgradeCachesInvalidated() {
		List.copyOf(upgradeCachesInvalidatedListeners).forEach(Runnable::run);
	}

	public Collection<IStorageWrapper> getSubBackpacks() {
		return subBackpacks.values();
	}

	public void saveInitializedSubBackpacks() {
		List<Integer> initializedSlots = subBackpacks.entrySet().stream().filter(entry -> entry.getValue().getContentsUuid().isPresent()).map(Map.Entry::getKey)
				.toList();
		if (initializedSlots.isEmpty()) {
			return;
		}

		boolean savedOrdinaryBackpack = false;
		for (int slot : initializedSlots) {
			if (LinkedStorageStackLifecycle.classifyEndpoint(inventoryHandler.getStackInSlot(slot)) == LinkedStorageEndpointStackState.ENDPOINT) {
				continue;
			}
			inventoryHandler.setStackInSlot(slot, ((IBackpackWrapper) subBackpacks.get(slot)).getBackpack());
			savedOrdinaryBackpack = true;
		}
		if (savedOrdinaryBackpack) {
			inventoryHandler.saveInventory();
		}
	}

	private void onContentsChanged(int slot) {
		ItemStack stackInSlot = inventoryHandler.getStackInSlot(slot);
		boolean backpackIsInTheSlot = stackInSlot.getItem() instanceof BackpackItem;
		boolean backpackWasInTheSlot = subBackpacks.containsKey(slot);
		if (!backpackWasInTheSlot && !backpackIsInTheSlot) {
			return;
		}

		if (backpackWasInTheSlot != backpackIsInTheSlot) {
			notifyAndRefreshSubbackpacks();
		} else {
			LazyOptional<IBackpackWrapper> backpackWrapper = stackInSlot.getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
			if (backpackWrapper.map(w -> w != subBackpacks.get(slot)).orElse(true)) {
				notifyAndRefreshSubbackpacks();
			}
		}
	}

	private void notifyAndRefreshSubbackpacks() {
		refreshSubBackpacksIfChanged();
	}

	private void notifyAfterRefresh() {
		runRefreshListeners(refreshListeners);
	}

	private void runRefreshListeners(Set<Consumer<Collection<IStorageWrapper>>> refreshListeners) {
		for (Consumer<Collection<IStorageWrapper>> refreshListener : List.copyOf(refreshListeners)) {
			refreshListener.accept(subBackpacks.values());
		}
	}

	private void notifyBeforeRefresh() {
		runRefreshListeners(beforeRefreshListeners);
	}

	private Map<Integer, IStorageWrapper> getSubBackpacksFromInventory() {
		Map<Integer, IStorageWrapper> refreshed = new LinkedHashMap<>();
		for (int slot = 0; slot < inventoryHandler.getSlots(); slot++) {
			int finalSlot = slot;
			ItemStack stack = inventoryHandler.getStackInSlot(slot);
			if (stack.getItem() instanceof BackpackItem) {
				stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(wrapper -> refreshed.put(finalSlot, wrapper));
			}
		}
		return refreshed;
	}

	public void addBeforeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		beforeRefreshListeners.add(listener);
	}

	public void removeBeforeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		beforeRefreshListeners.remove(listener);
	}
}
