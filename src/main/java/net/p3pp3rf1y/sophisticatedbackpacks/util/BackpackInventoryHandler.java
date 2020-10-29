package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import javax.annotation.Nullable;
import java.util.UUID;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private boolean persistent = false;
	private UUID playerUuid;
	private String handlerName;
	private int backpackSlot;

	public BackpackInventoryHandler(ItemStack backpack) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	public void setPersistent(@Nullable UUID playerUuid, @Nullable String handlerName, int backpackSlot) {
		this.playerUuid = playerUuid;
		this.handlerName = handlerName;
		this.backpackSlot = backpackSlot;
		persistent = true;
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		if (persistent) {
			backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
			if (playerUuid != null) {
				BackpackInventoryEventBus.onSlotUpdate(playerUuid, handlerName, backpackSlot, slot, getStackInSlot(slot));
			}
		}
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public void onInventorySlotUpdate(int slot, ItemStack newStack) {
		setStackInSlot(slot, newStack);
	}
}
