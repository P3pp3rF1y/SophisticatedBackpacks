package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.IntNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

import javax.annotation.Nullable;
import java.util.UUID;

public class BackpackWrapper {
	private static final int DEFAULT_COLOR = 12999733;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private final ItemStack backpack;
	private boolean persistent = false;
	private BackpackInventoryHandler handler = null;
	private BackpackUpgradeHandler upgradeHandler = null;
	private UUID playerUuid = null;
	private String handlerName = null;
	private int backpackSlot = -1;

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	public void setPersistent() {
		persistent = true;
	}

	public void setNotificationData(@Nullable UUID playerUuid, @Nullable String handlerName, int backpackSlot) {
		this.playerUuid = playerUuid;
		this.handlerName = handlerName;
		this.backpackSlot = backpackSlot;
		setPersistent();
	}

	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(backpack);
			if (persistent) {
				handler.setPersistent(playerUuid, handlerName, backpackSlot);
			}
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
			upgradeHandler = new BackpackUpgradeHandler(backpack, persistent);
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
	}

	public void onInventorySlotUpdate(int slot, ItemStack newStack) {
		getInventoryHandler().onInventorySlotUpdate(slot, newStack);
	}
}
