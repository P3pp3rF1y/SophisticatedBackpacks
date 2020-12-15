package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.IntNBT;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;

import java.util.Optional;
import java.util.function.Consumer;

public class BackpackWrapper implements IBackpackWrapper {
	@CapabilityInject(IBackpackWrapper.class)
	public static Capability<IBackpackWrapper> BACKPACK_WRAPPER_CAPABILITY = null;

	public static final int DEFAULT_CLOTH_COLOR = 13394234;
	public static final int DEFAULT_BORDER_COLOR = 6434330;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private final ItemStack backpack;
	private BackpackInventoryHandler handler = null;
	private BackpackUpgradeHandler upgradeHandler = null;
	private Consumer<ItemStack> backpackSaveHandler = stack -> {};

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	@Override
	public void linkToTileEntity(BackpackTileEntity te) {
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
	public IItemHandler getFilteredHandler() {
		return getUpgradeHandler().getFilteredHandler(getInventoryHandler());
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
		return NBTHelper.getInt(backpack, CLOTH_COLOR_TAG).orElse(DEFAULT_CLOTH_COLOR);
	}

	@Override
	public int getBorderColor() {
		return NBTHelper.getInt(backpack, BORDER_COLOR_TAG).orElse(DEFAULT_BORDER_COLOR);
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		return NBTHelper.getInt(backpack, OPEN_TAB_ID_TAG);
	}

	@Override
	public void setOpenTabId(int openTabId) {
		NBTHelper.setInteger(backpack, OPEN_TAB_ID_TAG, openTabId);
		backpackSaveHandler.accept(backpack);
	}

	@Override
	public void removeOpenTabId() {
		backpack.getOrCreateTag().remove(OPEN_TAB_ID_TAG);
		backpackSaveHandler.accept(backpack);
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
