package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraftforge.common.util.LazyOptional;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;

public class SubBackpacksHandler {
	private final Map<Integer, IBackpackWrapper> subBackpacks = new LinkedHashMap<>();

	private final BackpackInventoryHandler inventoryHandler;
	private final Set<Consumer<Collection<IBackpackWrapper>>> refreshListeners = new HashSet<>();
	private final Set<Consumer<Collection<IBackpackWrapper>>> beforeRefreshListeners = new HashSet<>();

	public SubBackpacksHandler(BackpackInventoryHandler inventoryHandler) {
		this.inventoryHandler = inventoryHandler;
		this.inventoryHandler.addListener(this::onContentsChanged);

		refreshSubBackpacks();
	}

	public void addRefreshListener(Consumer<Collection<IBackpackWrapper>> listener) {
		refreshListeners.add(listener);
	}

	public Collection<IBackpackWrapper> getSubBackpacks() {
		return subBackpacks.values();
	}

	private void onContentsChanged(int slot) {
		LazyOptional<IBackpackWrapper> backpackWrapper = inventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
		if (subBackpacks.containsKey(slot) != backpackWrapper.isPresent() || backpackWrapper.map(w -> w != subBackpacks.get(slot)).orElse(false)) {
			notifyBeforeRefresh();
			refreshSubBackpacks();
			notifyAfterRefresh();
		}
	}

	private void notifyAfterRefresh() {
		runRefreshListeners(refreshListeners);
	}

	private void runRefreshListeners(Set<Consumer<Collection<IBackpackWrapper>>> refreshListeners) {
		for (Consumer<Collection<IBackpackWrapper>> refreshListener : refreshListeners) {
			refreshListener.accept(subBackpacks.values());
		}
	}

	private void notifyBeforeRefresh() {
		runRefreshListeners(beforeRefreshListeners);
	}

	private void refreshSubBackpacks() {
		subBackpacks.clear();

		for (int slot = 0; slot < inventoryHandler.getSlots(); slot++) {
			int finalSlot = slot;
			inventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> subBackpacks.put(finalSlot, wrapper));
		}
	}

	public void addBeforeRefreshListener(Consumer<Collection<IBackpackWrapper>> listener) {
		beforeRefreshListeners.add(listener);
	}
}
