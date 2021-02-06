package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.IntNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventorySorter;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackKey;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Comparator;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public class BackpackWrapper implements IBackpackWrapper {
	public static final int DEFAULT_CLOTH_COLOR = 13394234;
	public static final int DEFAULT_BORDER_COLOR = 6434330;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private static final String SORT_BY_TAG = "sortBy";
	private static final String CONTENTS_UUID_TAG = "contentsUuid";
	private static final String INVENTORY_SLOTS_TAG = "inventorySlots";
	private static final String UPGRADE_SLOTS_TAG = "upgradeSlots";
	private static final String ORIGINAL_UUID_TAG = "originalUuid";

	private final ItemStack backpack;
	private Runnable backpackSaveHandler = () -> {};

	@Nullable
	private BackpackInventoryHandler handler = null;
	@Nullable
	private BackpackUpgradeHandler upgradeHandler = null;
	@Nullable
	private InventoryIOHandler inventoryIOHandler = null;
	@Nullable
	private InventoryModificationHandler inventoryModificationHandler = null;

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
	}

	@Override
	public void setBackpackSaveHandler(Runnable saveHandler) {
		backpackSaveHandler = saveHandler;
		refreshInventoryForUpgradeProcessing();
	}

	@Override
	public IItemHandlerModifiable getInventoryForUpgradeProcessing() {
		if (inventoryModificationHandler == null) {
			inventoryModificationHandler = new InventoryModificationHandler(this);
		}
		return inventoryModificationHandler.getModifiedInventoryHandler();
	}

	@Override
	public BackpackInventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(getNumberOfInventorySlots(), this, getBackpackContentsNbt(), this::markBackpackContentsDirty);
		}
		return handler;
	}

	private int getNumberOfInventorySlots() {
		return NBTHelper.getInt(backpack, INVENTORY_SLOTS_TAG).orElse(((BackpackItem) backpack.getItem()).getNumberOfSlots());
	}

	private CompoundNBT getBackpackContentsNbt() {
		return BackpackStorage.get().getOrCreateBackpackContents(getOrCreateContentsUuid(), getOriginalUuid());
	}

	@Nullable
	private UUID getOriginalUuid() {
		return NBTHelper.getUniqueId(backpack, ORIGINAL_UUID_TAG).orElse(null);
	}

	private void markBackpackContentsDirty() {
		BackpackStorage.get().markDirty();
	}

	@Override
	public IItemHandlerModifiable getInventoryForInputOutput() {
		if (inventoryIOHandler == null) {
			inventoryIOHandler = new InventoryIOHandler(this);
		}
		return inventoryIOHandler.getFilteredItemHandler();
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		otherBackpackWrapper.setOriginalUuid(getOrCreateContentsUuid());

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
			upgradeHandler = new BackpackUpgradeHandler(getNumberOfUpgradeSlots(), this, getBackpackContentsNbt(), this::markBackpackContentsDirty, () -> {
				getInventoryHandler().clearListeners();
				inventoryIOHandler = null;
				inventoryModificationHandler = null;
			});
		}
		return upgradeHandler;
	}

	private int getNumberOfUpgradeSlots() {
		return NBTHelper.getInt(backpack, UPGRADE_SLOTS_TAG).orElse(((BackpackItem) backpack.getItem()).getNumberOfUpgradeSlots());
	}

	@Override
	public CompoundNBT getClientTag() {
		CompoundNBT tag = backpack.getOrCreateTag();
		tag.putInt(INVENTORY_SLOTS_TAG, getInventoryHandler().getSlots());
		tag.putInt(UPGRADE_SLOTS_TAG, getUpgradeHandler().getSlots());
		return tag;
	}

	@Override
	public UUID getOrCreateContentsUuid() {
		Optional<UUID> contentsUuid = NBTHelper.getUniqueId(backpack, CONTENTS_UUID_TAG);
		if (contentsUuid.isPresent()) {
			return contentsUuid.get();
		}
		UUID newUuid = UUID.randomUUID();
		NBTHelper.setUniqueId(backpack, CONTENTS_UUID_TAG, newUuid);
		migrateBackpackContents(newUuid);
		return newUuid;
	}

	private void migrateBackpackContents(UUID newUuid) {
		migrateNbtTag(newUuid, BackpackInventoryHandler.INVENTORY_TAG);
		migrateNbtTag(newUuid, BackpackUpgradeHandler.UPGRADE_INVENTORY_TAG);
	}

	private void migrateNbtTag(UUID newUuid, String key) {
		NBTHelper.getCompound(backpack, key)
				.ifPresent(nbt -> {
					BackpackStorage.get().getOrCreateBackpackContents(newUuid).put(key, nbt);
					markBackpackContentsDirty();
					NBTHelper.removeTag(backpack, key);
				});
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
		backpackSaveHandler.run();
	}

	@Override
	public void removeOpenTabId() {
		backpack.getOrCreateTag().remove(OPEN_TAB_ID_TAG);
		backpackSaveHandler.run();
	}

	@Override
	public void setColors(int clothColor, int borderColor) {
		backpack.setTagInfo(CLOTH_COLOR_TAG, IntNBT.valueOf(clothColor));
		backpack.setTagInfo(BORDER_COLOR_TAG, IntNBT.valueOf(borderColor));
		backpackSaveHandler.run();
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		backpack.setTagInfo(SORT_BY_TAG, StringNBT.valueOf(sortBy.getString()));
		backpackSaveHandler.run();
	}

	@Override
	public SortBy getSortBy() {
		return NBTHelper.getEnumConstant(backpack, SORT_BY_TAG, SortBy::fromName).orElse(SortBy.NAME);
	}

	@Override
	public void sort() {
		InventorySorter.sortHandler(getInventoryHandler(), getComparator());
	}

	private Comparator<Map.Entry<ItemStackKey, Integer>> getComparator() {
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

	@Override
	public ItemStack cloneBackpack() {
		ItemStack clonedBackpack = cloneBackpack(this);
		clonedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(this::cloneSubbackpacks);
		return clonedBackpack;
	}

	private void cloneSubbackpacks(IBackpackWrapper wrapperCloned) {
		BackpackInventoryHandler inventoryHandler = wrapperCloned.getInventoryHandler();
		InventoryHelper.iterate(inventoryHandler, (slot, stack) -> {
			if (!(stack.getItem() instanceof BackpackItem)) {
				return;
			}
			inventoryHandler.setStackInSlot(slot,
					stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(this::cloneBackpack).orElse(ItemStack.EMPTY));
		});
	}

	private ItemStack cloneBackpack(IBackpackWrapper originalWrapper) {
		ItemStack backpackCopy = new ItemStack(originalWrapper.getBackpack().getItem());
		return backpackCopy.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapperCopy -> {
							originalWrapper.copyDataTo(wrapperCopy);
							wrapperCopy.removeLinkToOriginalBackpack();
							return wrapperCopy.getBackpack();
						}
				).orElse(ItemStack.EMPTY);
	}

	@Override
	public void refreshInventoryForInputOutput() {
		inventoryIOHandler = null;
	}

	@Override
	public void setOriginalUuid(UUID originalUuid) {
		NBTHelper.setUniqueId(backpack, ORIGINAL_UUID_TAG, originalUuid);
		backpackSaveHandler.run();
	}

	@Override
	public void removeOriginalBackpack() {
		BackpackStorage.get().removeOriginalBackpack(getOriginalUuid());
		removeLinkToOriginalBackpack();
		backpackSaveHandler.run();
	}

	@Override
	public void removeLinkToOriginalBackpack() {
		NBTHelper.getUniqueId(backpack, ORIGINAL_UUID_TAG).ifPresent(originalUuid -> BackpackStorage.get().removeLinkToOriginalBackpack(originalUuid));
		NBTHelper.removeTag(backpack, ORIGINAL_UUID_TAG);
		backpackSaveHandler.run();
	}

	@Override
	public void setPersistent(boolean persistent) {
		getInventoryHandler().setPersistent(persistent);
		getUpgradeHandler().setPersistent(persistent);
	}

	@Override
	public void refreshInventoryForUpgradeProcessing() {
		inventoryModificationHandler = null;
		refreshInventoryForInputOutput();
	}

	@Override
	public void onContentsNbtUpdated() {
		handler = null;
		upgradeHandler = null;
		refreshInventoryForUpgradeProcessing();
	}
}
