package net.p3pp3rf1y.sophisticatedbackpacks.compat.inventorysorter;

import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.InterModComms;
import net.minecraftforge.fml.event.lifecycle.InterModEnqueueEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackInventorySlot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolFilterSlot;

public class InventorySorterCompat implements ICompat {
	private static final String SLOTBLACKLIST = "slotblacklist";

	public InventorySorterCompat() {
		IEventBus modEventBus = FMLJavaModLoadingContext.get().getModEventBus();
		modEventBus.addListener(this::sendImc);
	}

	private void sendImc(InterModEnqueueEvent evt) {
		evt.enqueueWork(() -> {
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, BackpackContainer.BackpackUpgradeSlot.class::getName);
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, FilterSlotItemHandler.class::getName);
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, FilterLogicContainer.FilterLogicSlot.class::getName);
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, SlotSuppliedHandler.class::getName);
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, ToolFilterSlot.class::getName);
			InterModComms.sendTo(CompatModIds.INVENTORY_SORTER, SLOTBLACKLIST, BackpackInventorySlot.class::getName);
		});
	}

	@Override
	public void setup() {
		//noop
	}
}
