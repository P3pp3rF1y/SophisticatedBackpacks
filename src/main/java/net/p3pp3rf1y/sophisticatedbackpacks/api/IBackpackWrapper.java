package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
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

	BackpackUpgradeHandler getUpgradeHandler();

	CompoundNBT getClientTag();

	void removeContentsUuid();

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

	void setOriginalUuid(UUID orCreateContentsUuid);

	void removeOriginalBackpack();

	void removeLinkToOriginalBackpack();

	void setPersistent(boolean persistent);
}
