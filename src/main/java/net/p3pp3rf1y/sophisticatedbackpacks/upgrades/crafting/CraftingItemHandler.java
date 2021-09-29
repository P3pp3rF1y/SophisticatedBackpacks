package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.player.StackedContents;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class CraftingItemHandler extends CraftingContainer {
	private final Supplier<IItemHandlerModifiable> supplyInventory;
	private final Consumer<Container> onCraftingMatrixChanged;

	public CraftingItemHandler(Supplier<IItemHandlerModifiable> supplyInventory, Consumer<Container> onCraftingMatrixChanged) {
		super(new AbstractContainerMenu(null, -1) {
			@Override
			public boolean stillValid(Player playerIn) {
				return false;
			}
		}, 3, 3);
		this.supplyInventory = supplyInventory;
		this.onCraftingMatrixChanged = onCraftingMatrixChanged;
	}

	@Override
	public int getContainerSize() {
		return supplyInventory.get().getSlots();
	}

	@Override
	public boolean isEmpty() {
		return InventoryHelper.isEmpty(supplyInventory.get());
	}

	@Override
	public ItemStack getItem(int index) {
		IItemHandlerModifiable itemHandler = supplyInventory.get();
		return index >= itemHandler.getSlots() ? ItemStack.EMPTY : itemHandler.getStackInSlot(index);
	}

	@Override
	public ItemStack removeItemNoUpdate(int index) {
		return InventoryHelper.getAndRemove(supplyInventory.get(), index);
	}

	@Override
	public ItemStack removeItem(int index, int count) {
		ItemStack itemstack = supplyInventory.get().extractItem(index, count, false);
		if (!itemstack.isEmpty()) {
			onCraftingMatrixChanged.accept(this);
		}

		return itemstack;
	}

	@Override
	public void setItem(int index, ItemStack stack) {
		supplyInventory.get().setStackInSlot(index, stack);
		onCraftingMatrixChanged.accept(this);
	}

	@Override
	public void fillStackedContents(StackedContents helper) {
		InventoryHelper.iterate(supplyInventory.get(), (slot, stack) -> helper.accountSimpleStack(stack));
	}

}
