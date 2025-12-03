package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;

import java.util.*;
import java.util.function.Consumer;

public class SubBackpacksHandler {
	private final Map<Integer, IStorageWrapper> subBackpacks = new LinkedHashMap<>();

	private final InventoryHandler inventoryHandler;
	private final Set<Consumer<Collection<IStorageWrapper>>> refreshListeners = new HashSet<>();
	private final Set<Consumer<Collection<IStorageWrapper>>> beforeRefreshListeners = new HashSet<>();

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

	private void onContentsChanged(int slot) {
		boolean backpackIsInTheSlot = inventoryHandler.getResource(slot).getItem() instanceof BackpackItem;
		boolean backpackWasInTheSlot = subBackpacks.containsKey(slot);
		if (!backpackWasInTheSlot && !backpackIsInTheSlot) {
			return;
		}

		if (backpackWasInTheSlot != backpackIsInTheSlot) {
			notifyAndRefreshSubbackpacks();
		} else {
			if (BackpackWrapper.fromStack(inventoryHandler.getStackInSlot(slot)) != subBackpacks.get(slot)) {
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

		for (int slot = 0; slot < inventoryHandler.size(); slot++) {
			ItemStack slotStack = inventoryHandler.getStackInSlot(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				subBackpacks.put(slot, BackpackWrapper.fromStack(slotStack));
			}
		}
	}

	public void addBeforeRefreshListener(Consumer<Collection<IStorageWrapper>> listener) {
		beforeRefreshListeners.add(listener);
	}
}
