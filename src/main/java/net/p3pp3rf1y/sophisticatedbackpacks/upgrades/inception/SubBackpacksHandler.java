package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.IntConsumer;

public class SubBackpacksHandler {
	private final Map<Integer, IStorageWrapper> subBackpacks = new LinkedHashMap<>();

	private final InventoryHandler inventoryHandler;
	private final IntConsumer contentsChangedListener = this::onContentsChanged;
	private final Set<Consumer<Collection<IStorageWrapper>>> refreshListeners = new HashSet<>();
	private final Set<Consumer<Collection<IStorageWrapper>>> beforeRefreshListeners = new HashSet<>();
	private final Set<Runnable> upgradeCachesInvalidatedListeners = new HashSet<>();
	private long lastWrapperCacheChangeCounter;

	public SubBackpacksHandler(InventoryHandler inventoryHandler) {
		this.inventoryHandler = inventoryHandler;
		ensureInventoryListenerRegistered();
	}

	public void ensureInventoryListenerRegistered() {
		inventoryHandler.removeListener(contentsChangedListener);
		inventoryHandler.addListener(contentsChangedListener);
		refreshSubBackpacksIfChanged();
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

	public void refresh() {
		long wrapperCacheChangeCounter = StorageWrapperRepository.getWrapperCacheChangeCounter();
		if (lastWrapperCacheChangeCounter == wrapperCacheChangeCounter) {
			return;
		}

		refreshSubBackpacksIfChanged();
	}

	private void refreshSubBackpacksIfChanged() {
		Map<Integer, IStorageWrapper> refreshedSubBackpacks = getSubBackpacksFromInventory();
		lastWrapperCacheChangeCounter = StorageWrapperRepository.getWrapperCacheChangeCounter();
		if (subBackpacks.equals(refreshedSubBackpacks)) {
			return;
		}

		notifyBeforeRefresh();
		subBackpacks.clear();
		subBackpacks.putAll(refreshedSubBackpacks);
		notifyAfterRefresh();
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
			if (BackpackWrapper.fromStack(stackInSlot) != subBackpacks.get(slot)) {
				notifyAndRefreshSubbackpacks();
			}
		}
	}

	private void notifyAndRefreshSubbackpacks() {
		notifyBeforeRefresh();
		refreshSubBackpacks();
		notifyAfterRefresh();
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

	private void refreshSubBackpacks() {
		subBackpacks.clear();
		subBackpacks.putAll(getSubBackpacksFromInventory());
		lastWrapperCacheChangeCounter = StorageWrapperRepository.getWrapperCacheChangeCounter();
	}

	private Map<Integer, IStorageWrapper> getSubBackpacksFromInventory() {
		Map<Integer, IStorageWrapper> refreshedSubBackpacks = new LinkedHashMap<>();

		for (int slot = 0; slot < inventoryHandler.getSlots(); slot++) {
			ItemStack slotStack = inventoryHandler.getStackInSlot(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				refreshedSubBackpacks.put(slot, BackpackWrapper.fromStack(slotStack));
			}
		}

		return refreshedSubBackpacks;
	}

	public void addBeforeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		beforeRefreshListeners.add(listener);
	}

	public void removeBeforeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		beforeRefreshListeners.remove(listener);
	}
}
