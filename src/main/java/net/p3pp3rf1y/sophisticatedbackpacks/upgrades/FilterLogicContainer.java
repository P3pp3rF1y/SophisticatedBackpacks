package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.inventory.container.Slot;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class FilterLogicContainer<T extends FilterLogic> extends FilterLogicContainerBase<T, FilterLogicContainer.FilterLogicSlot> {
	public FilterLogicContainer(Supplier<T> filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		super(serverUpdater, filterLogic);
		ItemStackHandler filterHandler = filterLogic.get().getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> {
			FilterLogicSlot filterSlot = new FilterLogicSlot(() -> filterLogic.get().getFilterHandler(), slot);
			addSlot.accept(filterSlot);
			filterSlots.add(filterSlot);
		});
	}

	public static class FilterLogicSlot extends FilterSlotItemHandler {
		private boolean enabled = true;

		public FilterLogicSlot(Supplier<IItemHandler> filterHandler, Integer slot) {
			super(filterHandler, slot, -100, -100);
		}

		public void setEnabled(boolean enabled) {
			this.enabled = enabled;
		}

		@Override
		public boolean isActive() {
			return enabled;
		}
	}
}
