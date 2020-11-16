package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.IntNBT;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

import java.util.function.Consumer;

public class BackpackWrapper implements IBackpackWrapper {
	@CapabilityInject(IBackpackWrapper.class)
	public static Capability<IBackpackWrapper> BACKPACK_WRAPPER_CAPABILITY = null;

	public static final int DEFAULT_COLOR = 13394234;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private final ItemStack backpack;
	private BackpackInventoryHandler handler = null;
	private BackpackUpgradeHandler upgradeHandler = null;
	private Consumer<ItemStack> backpackSaveHandler = stack -> {};

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	public BackpackWrapper(ItemStack backpack, BackpackTileEntity te) {
		this(backpack);
		backpackSaveHandler = stack -> te.markDirty();
	}

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(backpack, backpackSaveHandler);
		}
		return handler;
	}

	@Override
	public ScreenProperties getScreenProperties() {
		return ((BackpackItem) backpack.getItem()).getScreenProperties();
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		if (backpack.hasDisplayName()) {
			otherBackpackWrapper.getBackpack().setDisplayName(backpack.getDisplayName());
		}
		getInventoryHandler().copyStacksTo(otherBackpackWrapper.getInventoryHandler());
		getUpgradeHandler().copyTo(otherBackpackWrapper.getUpgradeHandler());
		otherBackpackWrapper.setColors(getClothColor(), getBorderColor());
	}

	@Override
	public BackpackUpgradeHandler getUpgradeHandler() {
		if (upgradeHandler == null) {
			upgradeHandler = new BackpackUpgradeHandler(backpack, backpackSaveHandler);
		}
		return upgradeHandler;
	}

	@Override
	public int getClothColor() {
		return NBTHelper.getInt(backpack, CLOTH_COLOR_TAG).orElse(DEFAULT_COLOR);
	}

	@Override
	public int getBorderColor() {
		return NBTHelper.getInt(backpack, BORDER_COLOR_TAG).orElse(DEFAULT_COLOR);
	}

	@Override
	public void setColors(int clothColor, int borderColor) {
		backpack.setTagInfo(CLOTH_COLOR_TAG, IntNBT.valueOf(clothColor));
		backpack.setTagInfo(BORDER_COLOR_TAG, IntNBT.valueOf(borderColor));
		backpackSaveHandler.accept(backpack);
	}

	@Override
	public ItemStack getBackpack() {
		return backpack;
	}
}
