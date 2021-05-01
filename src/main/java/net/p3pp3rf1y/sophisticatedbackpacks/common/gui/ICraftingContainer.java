package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.Slot;

import java.util.List;

public interface ICraftingContainer {
	List<Slot> getRecipeSlots();

	IInventory getCraftMatrix();
}
