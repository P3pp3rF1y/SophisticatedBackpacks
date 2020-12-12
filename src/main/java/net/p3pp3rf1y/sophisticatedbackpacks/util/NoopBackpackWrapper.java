package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;

import java.util.Optional;

public class NoopBackpackWrapper implements IBackpackWrapper {
	public static final NoopBackpackWrapper INSTANCE = new NoopBackpackWrapper();

	private NoopBackpackWrapper() {}

	private final ItemStack backpack = new ItemStack(ModItems.BACKPACK.get());

	@Override
	public void linkToTileEntity(BackpackTileEntity te) {
		//noop
	}

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		return new BackpackInventoryHandler(backpack, s -> {});
	}

	@Override
	public IItemHandler getFilteredHandler() {
		return new BackpackInventoryHandler(backpack, s -> {});
	}

	@Override
	public ScreenProperties getScreenProperties() {
		return new ScreenProperties();
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		//noop
	}

	@Override
	public BackpackUpgradeHandler getUpgradeHandler() {
		return new BackpackUpgradeHandler(backpack, s -> {});
	}

	@Override
	public int getClothColor() {
		return -1;
	}

	@Override
	public int getBorderColor() {
		return -1;
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		return Optional.empty();
	}

	@Override
	public void setOpenTabId(int openTabId) {
		//noop
	}

	@Override
	public void removeOpenTabId() {
		//noop
	}

	@Override
	public void setColors(int clothColor, int borderColor) {
		//noop
	}

	@Override
	public ItemStack getBackpack() {
		return backpack;
	}
}
