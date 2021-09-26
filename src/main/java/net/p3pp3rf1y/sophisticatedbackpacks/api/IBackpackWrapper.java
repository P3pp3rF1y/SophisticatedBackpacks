package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackSettingsHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackUpgradeHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;

import java.util.Optional;
import java.util.UUID;

public interface IBackpackWrapper {

	void setBackpackSaveHandler(Runnable saveHandler);

	IItemHandlerModifiable getInventoryForUpgradeProcessing();

	BackpackInventoryHandler getInventoryHandler();

	IItemHandlerModifiable getInventoryForInputOutput();

	void copyDataTo(IBackpackWrapper otherBackpackWrapper);

	BackpackSettingsHandler getSettingsHandler();

	BackpackUpgradeHandler getUpgradeHandler();

	Optional<UUID> getContentsUuid();

	int getClothColor();

	int getBorderColor();

	Optional<Integer> getOpenTabId();

	void setOpenTabId(int openTabId);

	void removeOpenTabId();

	void setColors(int clothColor, int borderColor);

	void setSortBy(SortBy sortBy);

	SortBy getSortBy();

	ItemStack getBackpack();

	ItemStack cloneBackpack();

	void sort();

	void onContentsNbtUpdated();

	void refreshInventoryForUpgradeProcessing();

	void refreshInventoryForInputOutput();

	void setPersistent(boolean persistent);

	void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots);

	void setLoot(ResourceLocation lootTableName, float lootPercentage);

	void fillWithLoot(Player playerEntity);

	void setContentsUuid(UUID backpackUuid);

	BackpackRenderInfo getRenderInfo();

	void setColumnsTaken(int columnsTaken);

	int getColumnsTaken();

	default int getNumberOfSlotRows() {
		return 0;
	}

	default Optional<IFluidHandlerItem> getFluidHandler() {
		return Optional.empty();
	}

	default Optional<IEnergyStorage> getEnergyStorage() { return Optional.empty(); }
}
