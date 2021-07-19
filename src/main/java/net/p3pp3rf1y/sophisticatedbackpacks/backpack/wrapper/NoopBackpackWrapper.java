package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Optional;
import java.util.UUID;

@SuppressWarnings("java:S4144") //this is noop wrapper and thus identical implementation isn't an issue especially when it means just returning same field
public class NoopBackpackWrapper implements IBackpackWrapper {
	public static final NoopBackpackWrapper INSTANCE = new NoopBackpackWrapper();

	private final ItemStack backpack = new ItemStack(ModItems.BACKPACK.get());
	private final BackpackSettingsHandler settingsHandler = new BackpackSettingsHandler(new CompoundNBT(), () -> {});
	private final BackpackUpgradeHandler backpackUpgradeHandler = new BackpackUpgradeHandler(0, this, new CompoundNBT(), () -> {}, () -> {});
	private final BackpackInventoryHandler backpackInventoryHandler = new BackpackInventoryHandler(0, this, new CompoundNBT(), () -> {}, 64);
	private final BackpackRenderInfo backpackRenderInfo = new BackpackRenderInfo(backpack, () -> () -> {});

	private NoopBackpackWrapper() {}

	@Override
	public void setBackpackSaveHandler(Runnable saveHandler) {
		//noop
	}

	@Override
	public BackpackInventoryHandler getInventoryForUpgradeProcessing() {
		return backpackInventoryHandler;
	}

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		return backpackInventoryHandler;
	}

	@Override
	public IItemHandlerModifiable getInventoryForInputOutput() {
		return backpackInventoryHandler;
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		//noop
	}

	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		return settingsHandler;
	}

	@Override
	public BackpackUpgradeHandler getUpgradeHandler() {
		return backpackUpgradeHandler;
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return Optional.empty();
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
	public void setSortBy(SortBy sortBy) {
		//noop
	}

	@Override
	public SortBy getSortBy() {
		return SortBy.NAME;
	}

	@Override
	public ItemStack getBackpack() {
		return backpack;
	}

	@Override
	public void sort() {
		//noop
	}

	@Override
	public void onContentsNbtUpdated() {
		//noop
	}

	@Override
	public void refreshInventoryForUpgradeProcessing() {
		//noop
	}

	@Override
	public ItemStack cloneBackpack() {
		return backpack;
	}

	@Override
	public void refreshInventoryForInputOutput() {
		//noop
	}

	@Override
	public void setPersistent(boolean persistent) {
		//noop
	}

	@Override
	public void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots) {
		//noop
	}

	@Override
	public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
		//noop
	}

	@Override
	public void fillWithLoot(PlayerEntity playerEntity) {
		//noop
	}

	@Override
	public void setContentsUuid(UUID backpackUuid) {
		//noop
	}

	@Override
	public BackpackRenderInfo getRenderInfo() {
		return backpackRenderInfo;
	}

	@Override
	public void setColumnsTaken(int columnsTaken) {
		//noop
	}

	@Override
	public int getColumnsTaken() {
		return 0;
	}
}
