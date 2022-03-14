package net.p3pp3rf1y.sophisticatedcore.api;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.energy.IEnergyStorage;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.inventory.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import java.util.Optional;
import java.util.UUID;

public interface IStorageWrapper {

	void setSaveHandler(Runnable saveHandler);

	default void setInventorySlotChangeHandler(Runnable slotChangeHandler) {
		//noop
	}

	IItemHandlerSimpleInserter getInventoryForUpgradeProcessing();

	InventoryHandler getInventoryHandler();

	IItemHandlerSimpleInserter getInventoryForInputOutput();

	SettingsHandler getSettingsHandler();

	UpgradeHandler getUpgradeHandler();

	Optional<UUID> getContentsUuid();

	int getMainColor();

	int getAccentColor();

	Optional<Integer> getOpenTabId();

	void setOpenTabId(int openTabId);

	void removeOpenTabId();

	void setColors(int mainColor, int accentColor);

	void setSortBy(SortBy sortBy);

	SortBy getSortBy();

	void sort();

	void onContentsNbtUpdated();

	void refreshInventoryForUpgradeProcessing();

	void refreshInventoryForInputOutput();

	void setPersistent(boolean persistent);

	void fillWithLoot(Player playerEntity);

	RenderInfo getRenderInfo();

	void setColumnsTaken(int columnsTaken);

	int getColumnsTaken();

	default int getNumberOfSlotRows() {
		return 0;
	}

	default Optional<IStorageFluidHandler> getFluidHandler() {
		return Optional.empty();
	}

	default Optional<IEnergyStorage> getEnergyStorage() {return Optional.empty();}

	default ItemStack getWrappedStorageStack() {
		return ItemStack.EMPTY;
	}
}
