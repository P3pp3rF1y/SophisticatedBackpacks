package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.IntNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class BackpackWrapper {
	public static final int DEFAULT_COLOR = 13394234;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private final ItemStack backpack;
	private BackpackInventoryHandler handler = null;
	private BackpackUpgradeHandler upgradeHandler = null;
	private Consumer<ItemStack> backpackSaveHandler = stack -> {};
	private BiConsumer<Integer, Supplier<ItemStack>> notificationHandler = (slot, stack) -> {};

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	public void setPersistent(PlayerEntity player, String handlerName, int backpackSlot, boolean notifyOnUpdate) {
		backpackSaveHandler = stack -> PlayerInventoryProvider.getPlayerInventoryHandler(handlerName).ifPresent(h -> h.setStackInSlot(player, backpackSlot, stack));
		if (notifyOnUpdate) {
			notificationHandler = (slot, stack) -> BackpackInventoryEventBus.onSlotUpdate(player.getUniqueID(), handlerName, backpackSlot, slot, stack.get());
		}
	}

	public void setPersistent(BackpackTileEntity te) {
		backpackSaveHandler = stack -> te.markDirty();
	}

	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(backpack, backpackSaveHandler, notificationHandler);
		}
		return handler;
	}

	public ScreenProperties getScreenProperties() {
		return ((BackpackItem) backpack.getItem()).getScreenProperties();
	}

	public void copyDataTo(BackpackWrapper otherBackpackWrapper) {
		if (backpack.hasDisplayName()) {
			otherBackpackWrapper.backpack.setDisplayName(backpack.getDisplayName());
		}
		getInventoryHandler().copyStacksTo(otherBackpackWrapper.getInventoryHandler());
		getUpgradeHandler().copyTo(otherBackpackWrapper.getUpgradeHandler());
		otherBackpackWrapper.setColors(getClothColor(), getBorderColor());
	}

	public BackpackUpgradeHandler getUpgradeHandler() {
		if (upgradeHandler == null) {
			upgradeHandler = new BackpackUpgradeHandler(backpack, backpackSaveHandler);
		}
		return upgradeHandler;
	}

	public int getClothColor() {
		return NBTHelper.getInt(backpack, CLOTH_COLOR_TAG).orElse(DEFAULT_COLOR);
	}

	public int getBorderColor() {
		return NBTHelper.getInt(backpack, BORDER_COLOR_TAG).orElse(DEFAULT_COLOR);
	}

	public void setColors(int clothColor, int borderColor) {
		backpack.setTagInfo(CLOTH_COLOR_TAG, IntNBT.valueOf(clothColor));
		backpack.setTagInfo(BORDER_COLOR_TAG, IntNBT.valueOf(borderColor));
		backpackSaveHandler.accept(backpack);
	}

	public void onInventorySlotUpdate(int slot, ItemStack newStack) {
		getInventoryHandler().onInventorySlotUpdate(slot, newStack);
	}

	public ItemStack getBackpack() {
		return backpack;
	}

	public static BackpackWrapper getEmpty() {
		return new BackpackWrapper(new ItemStack(ModItems.BACKPACK));
	}
}
