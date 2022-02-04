package net.p3pp3rf1y.sophisticatedcore.inventory;

import net.p3pp3rf1y.sophisticatedcore.api.IIOFilterUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;

import java.util.ArrayList;
import java.util.List;

public class InventoryIOHandler {
	private final IItemHandlerSimpleInserter filteredItemHandler;

	public InventoryIOHandler(IStorageWrapper storageWrapper) {
		List<FilterLogic> inputFilters = new ArrayList<>();
		List<FilterLogic> outputFilters = new ArrayList<>();

		addFilters(storageWrapper, inputFilters, outputFilters);

		IItemHandlerSimpleInserter modifiedInventory = storageWrapper.getInventoryForUpgradeProcessing();
		if (inputFilters.isEmpty() && outputFilters.isEmpty()) {
			filteredItemHandler = modifiedInventory;
		} else {
			filteredItemHandler = new FilteredItemHandler.Modifiable(modifiedInventory, inputFilters, outputFilters);
		}
	}

	public IItemHandlerSimpleInserter getFilteredItemHandler() {
		return filteredItemHandler;
	}

	private void addFilters(IStorageWrapper storageWrapper, List<FilterLogic> inputFilters, List<FilterLogic> outputFilters) {
		List<IIOFilterUpgrade> filterWrappers = storageWrapper.getUpgradeHandler().getWrappersThatImplement(IIOFilterUpgrade.class);

		for (IIOFilterUpgrade wrapper : filterWrappers) {
			wrapper.getInputFilter().ifPresent(inputFilters::add);
			wrapper.getOutputFilter().ifPresent(outputFilters::add);
		}
	}
}
