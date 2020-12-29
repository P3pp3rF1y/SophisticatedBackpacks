package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IIOFilterUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;

import java.util.ArrayList;
import java.util.List;

public class InventoryIOHandler {
	private final IItemHandlerModifiable filteredItemHandler;

	public InventoryIOHandler(IBackpackWrapper backpackWrapper) {
		List<FilterLogic> inputFilters = new ArrayList<>();
		List<FilterLogic> outputFilters = new ArrayList<>();

		addFilters(backpackWrapper, inputFilters, outputFilters);

		IItemHandlerModifiable modifiedInventory = backpackWrapper.getInventoryForUpgradeProcessing();
		if (inputFilters.isEmpty() && outputFilters.isEmpty()) {
			filteredItemHandler = modifiedInventory;
		} else {
			filteredItemHandler = new FilteredItemHandler.Modifiable(modifiedInventory, inputFilters, outputFilters);
		}
	}

	public IItemHandlerModifiable getFilteredItemHandler() {
		return filteredItemHandler;
	}

	private void addFilters(IBackpackWrapper backpackWrapper, List<FilterLogic> inputFilters, List<FilterLogic> outputFilters) {
		List<IIOFilterUpgrade> filterWrappers = backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IIOFilterUpgrade.class);

		for (IIOFilterUpgrade wrapper : filterWrappers) {
			wrapper.getInputFilter().ifPresent(inputFilters::add);
			wrapper.getOutputFilter().ifPresent(outputFilters::add);
		}
	}
}
