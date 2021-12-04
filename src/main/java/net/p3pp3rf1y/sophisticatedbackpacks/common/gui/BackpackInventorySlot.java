package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;

import javax.annotation.Nonnull;

public class BackpackInventorySlot extends SlotItemHandler {
	private final boolean isClientSide;
	private final IBackpackWrapper backpackWrapper;
	private final BackpackInventoryHandler inventoryHandler;
	private final int slotIndex;

	private static IItemHandler wrapWithNoInsertAndExtractWrapper(IItemHandlerModifiable handler) {
		return new IItemHandlerModifiable() {
			@Override
			public void setStackInSlot(int slot, @Nonnull ItemStack stack) {
				handler.setStackInSlot(slot, stack);
			}

			@Override
			public int getSlots() {
				return handler.getSlots();
			}

			@Nonnull
			@Override
			public ItemStack getStackInSlot(int slot) {
				return handler.getStackInSlot(slot);
			}

			@Nonnull
			@Override
			public ItemStack insertItem(int slot, @Nonnull ItemStack stack, boolean simulate) {
				return stack;
			}

			@Nonnull
			@Override
			public ItemStack extractItem(int slot, int amount, boolean simulate) {
				return ItemStack.EMPTY;
			}

			@Override
			public int getSlotLimit(int slot) {
				return handler.getSlotLimit(slot);
			}

			@Override
			public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
				return handler.isItemValid(slot, stack);
			}
		};
	}

	public BackpackInventorySlot(boolean isClientSide, IBackpackWrapper backpackWrapper, BackpackInventoryHandler inventoryHandler, int slotIndex, int lineIndex, int yPosition) {
		super(wrapWithNoInsertAndExtractWrapper(inventoryHandler), slotIndex, 8 + lineIndex * 18, yPosition);
		this.isClientSide = isClientSide;
		this.backpackWrapper = backpackWrapper;
		this.inventoryHandler = inventoryHandler;
		this.slotIndex = slotIndex;
	}

	@Override
	public void setChanged() {
		super.setChanged();
		// saving here as well because there are many cases where vanilla modifies stack directly without and inventory handler isn't aware of it
		// however it does notify the slot of change
		backpackWrapper.getInventoryHandler().onContentsChanged(slotIndex);
		processSlotChangeResponse(slotIndex, backpackWrapper.getInventoryHandler(), backpackWrapper);
	}

	private void processSlotChangeResponse(int slot, IItemHandler handler, IBackpackWrapper backpackWrapper) {
		if (!isClientSide) {
			backpackWrapper.getUpgradeHandler().getWrappersThatImplementFromMainBackpack(ISlotChangeResponseUpgrade.class).forEach(u -> u.onSlotChange(handler, slot));
		}
	}

	@Override
	public int getMaxStackSize(ItemStack stack) {
		return inventoryHandler.getStackLimit(slotIndex, stack);
	}

	@Override
	public boolean mayPickup(PlayerEntity playerIn)
	{
		return !inventoryHandler.extractItem(slotIndex, 1, true).isEmpty();
	}

	@Override
	@Nonnull
	public ItemStack remove(int amount)
	{
		return inventoryHandler.extractItem(slotIndex, amount, false);
	}

}
