package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.StorageWrapperRepository;

import java.util.*;
import java.util.function.Consumer;

public class SubBackpacksHandler {
	private final Map<Integer, IStorageWrapper> subBackpacks = new LinkedHashMap<>();

	private final InventoryHandler inventoryHandler;
	private final Set<Consumer<Collection<IStorageWrapper>>> refreshListeners = new HashSet<>();
	private final Set<Consumer<Collection<IStorageWrapper>>> beforeRefreshListeners = new HashSet<>();
	private long lastWrapperCacheChangeCounter;

	public SubBackpacksHandler(InventoryHandler inventoryHandler) {
		this.inventoryHandler = inventoryHandler;
		this.inventoryHandler.addListener(this::onContentsChanged);

		refreshSubBackpacks();
	}

	public void addRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		refreshListeners.add(listener);
	}

	public Collection<IStorageWrapper> getSubBackpacks() {
		return subBackpacks.values();
	}

	public void refresh() {
		long wrapperCacheChangeCounter = StorageWrapperRepository.getWrapperCacheChangeCounter();
		if (lastWrapperCacheChangeCounter == wrapperCacheChangeCounter) {
			return;
		}

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
		for (Consumer<Collection<IStorageWrapper>> refreshListener : refreshListeners) {
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
}
