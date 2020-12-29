package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.Direction;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeWrapper;

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
		List<FilterUpgradeWrapper> filterWrappers = backpackWrapper.getUpgradeHandler().getTypeWrappers(FilterUpgradeItem.TYPE);

		for (FilterUpgradeWrapper wrapper : filterWrappers) {
			Direction dir = wrapper.getDirection();

			FilterLogic filterLogic = wrapper.getFilterLogic();
			switch (dir) {
				case BOTH:
					inputFilters.add(filterLogic);
					outputFilters.add(filterLogic);
					break;
				case INPUT:
					inputFilters.add(filterLogic);
					break;
				case OUTPUT:
					outputFilters.add(filterLogic);
					break;
			}
		}
	}
}
