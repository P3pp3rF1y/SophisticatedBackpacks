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

public class SubBackpacksHandler {
	private final Map<Integer, IBackpackWrapper> subBackpacks = new LinkedHashMap<>();

	private final BackpackInventoryHandler inventoryHandler;
	private final Set<Runnable> refreshListeners = new HashSet<>();

	public SubBackpacksHandler(BackpackInventoryHandler inventoryHandler) {
		this.inventoryHandler = inventoryHandler;
		this.inventoryHandler.addListener(this::onContentsChanged);

		refreshSubBackpacks();
	}

	public void addRefreshListener(Runnable listener) {
		refreshListeners.add(listener);
	}

	public Collection<IBackpackWrapper> getSubBackpacks() {
		return subBackpacks.values();
	}

	private void onContentsChanged(int slot) {
		LazyOptional<IBackpackWrapper> backpackWrapper = inventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance());
		if (subBackpacks.containsKey(slot) != backpackWrapper.isPresent() || backpackWrapper.map(w -> w != subBackpacks.get(slot)).orElse(false)) {
			refreshSubBackpacks();
			for (Runnable refreshListener : refreshListeners) {
				refreshListener.run();
			}
		}
	}

	private void refreshSubBackpacks() {
		subBackpacks.clear();

		for (int slot = 0; slot < inventoryHandler.getSlots(); slot++) {
			int finalSlot = slot;
			inventoryHandler.getStackInSlot(slot).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> subBackpacks.put(finalSlot, wrapper));
		}
	}
}
