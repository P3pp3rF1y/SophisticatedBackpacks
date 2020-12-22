package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.IntNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;

import java.util.Comparator;
import java.util.Map;
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
	private static final String SORT_BY_TAG = "sortBy";
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
	public IItemHandlerModifiable getInceptionInventoryHandler() {
		return getUpgradeHandler().getInceptionInventoryHandler(getInventoryHandler());
	}

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(backpack, backpackSaveHandler);
		}
		return handler;
	}

	@Override
	public IItemHandlerModifiable getFilteredHandler() {
		return getUpgradeHandler().getFilteredHandler(getInceptionInventoryHandler());
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
	public void setSortBy(SortBy sortBy) {
		backpack.setTagInfo(SORT_BY_TAG, StringNBT.valueOf(sortBy.getString()));
		backpackSaveHandler.accept(backpack);
	}

	@Override
	public SortBy getSortBy() {
		return NBTHelper.getEnumConstant(backpack, SORT_BY_TAG, SortBy::fromName).orElse(SortBy.NAME);
	}

	@Override
	public void sort() {
		InventorySorter.sortHandler(getInventoryHandler(), getComparator());
	}

	private Comparator<Map.Entry<InventorySorter.FilterStack, Integer>> getComparator() {
		switch (getSortBy()) {
			case COUNT:
				return InventorySorter.BY_COUNT;
			case TAGS:
				return InventorySorter.BY_TAGS;
			case NAME:
			default:
				return InventorySorter.BY_NAME;
		}
	}

	@Override
	public ItemStack getBackpack() {
		return backpack;
	}
}
