package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.FloatNBT;
import net.minecraft.nbt.IntNBT;
import net.minecraft.nbt.StringNBT;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyStorageUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventorySorter;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackKey;
import net.p3pp3rf1y.sophisticatedbackpacks.util.LootHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import javax.annotation.Nullable;
import java.util.Comparator;
import java.util.List;
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
	private static final String LOOT_TABLE_NAME_TAG = "lootTableName";
	private static final String LOOT_PERCENTAGE_TAG = "lootPercentage";
	private static final String COLUMNS_TAKEN_TAG = "columnsTaken";

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
	@Nullable
	private BackpackSettingsHandler settingsHandler = null;
	private boolean fluidHandlerInitialized = false;
	@Nullable
	private IFluidHandlerItem fluidHandler = null;
	private boolean energyStorageInitialized = false;
	@Nullable
	private IEnergyStorage energyStorage = null;

	private final BackpackRenderInfo renderInfo;

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
		renderInfo = new BackpackRenderInfo(backpack, () -> backpackSaveHandler);
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
			handler = new BackpackInventoryHandler(getNumberOfInventorySlots() - (getNumberOfSlotRows() * getColumnsTaken()),
					this, getBackpackContentsNbt(), this::markBackpackContentsDirty, StackUpgradeItem.getInventorySlotLimit(this));
		}
		return handler;
	}

	private int getNumberOfInventorySlots() {
		Optional<Integer> inventorySlots = NBTHelper.getInt(backpack, INVENTORY_SLOTS_TAG);

		if (inventorySlots.isPresent()) {
			return inventorySlots.get();
		}

		int itemInventorySlots = ((BackpackItem) backpack.getItem()).getNumberOfSlots();
		setNumberOfInventorySlots(itemInventorySlots);
		return itemInventorySlots;
	}

	@Override
	public int getNumberOfSlotRows() {
		int itemInventorySlots = getNumberOfInventorySlots();
		return (int) Math.ceil(itemInventorySlots <= 81 ? (double) itemInventorySlots / 9 : (double) itemInventorySlots / 12);
	}

	private void setNumberOfInventorySlots(int itemInventorySlots) {
		NBTHelper.setInteger(backpack, INVENTORY_SLOTS_TAG, itemInventorySlots);
	}

	private CompoundNBT getBackpackContentsNbt() {
		return BackpackStorage.get().getOrCreateBackpackContents(getOrCreateContentsUuid());
	}

	private void markBackpackContentsDirty() {
		BackpackStorage.get().setDirty();
	}

	@Override
	public IItemHandlerModifiable getInventoryForInputOutput() {
		if (inventoryIOHandler == null) {
			inventoryIOHandler = new InventoryIOHandler(this);
		}
		return inventoryIOHandler.getFilteredItemHandler();
	}

	@Override
	public Optional<IFluidHandlerItem> getFluidHandler() {
		if (!fluidHandlerInitialized) {
			IFluidHandlerItem wrappedHandler = getUpgradeHandler().getTypeWrappers(TankUpgradeItem.TYPE).isEmpty() ? null : new BackpackFluidHandler(this);
			List<IFluidHandlerWrapperUpgrade> fluidHandlerWrapperUpgrades = getUpgradeHandler().getWrappersThatImplement(IFluidHandlerWrapperUpgrade.class);

			for (IFluidHandlerWrapperUpgrade fluidHandlerWrapperUpgrade : fluidHandlerWrapperUpgrades) {
				wrappedHandler = fluidHandlerWrapperUpgrade.wrapHandler(wrappedHandler, backpack);
			}

			fluidHandler = wrappedHandler;
		}

		return Optional.ofNullable(fluidHandler);
	}

	@Override
	public Optional<IEnergyStorage> getEnergyStorage() {
		if (!energyStorageInitialized) {
			IEnergyStorage wrappedStorage = getUpgradeHandler().getWrappersThatImplement(IEnergyStorage.class).stream().findFirst().orElse(null);

			for (IEnergyStorageUpgradeWrapper energyStorageWrapperUpgrade : getUpgradeHandler().getWrappersThatImplement(IEnergyStorageUpgradeWrapper.class)) {
				wrappedStorage = energyStorageWrapperUpgrade.wrapStorage(wrappedStorage);
			}

			energyStorage = wrappedStorage;
		}

		return Optional.ofNullable(energyStorage);
	}

	@Override
	public void copyDataTo(IBackpackWrapper otherBackpackWrapper) {
		getContentsUuid().ifPresent(originalUuid -> {
			getInventoryHandler().copyStacksTo(otherBackpackWrapper.getInventoryHandler());
			getUpgradeHandler().copyTo(otherBackpackWrapper.getUpgradeHandler());
			getSettingsHandler().copyTo(otherBackpackWrapper.getSettingsHandler());
		});
	}

	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		if (settingsHandler == null) {
			if (getContentsUuid().isPresent()) {
				settingsHandler = new BackpackSettingsHandler(getBackpackContentsNbt(), this::markBackpackContentsDirty);
			} else {
				settingsHandler = NoopBackpackWrapper.INSTANCE.getSettingsHandler();
			}
		}
		return settingsHandler;
	}

	@Override
	public BackpackUpgradeHandler getUpgradeHandler() {
		if (upgradeHandler == null) {
			if (getContentsUuid().isPresent()) {
				upgradeHandler = new BackpackUpgradeHandler(getNumberOfUpgradeSlots(), this, getBackpackContentsNbt(), this::markBackpackContentsDirty, () -> {
					if (handler != null) {
						handler.clearListeners();
						handler.setSlotLimit(StackUpgradeItem.getInventorySlotLimit(this));
					}
					getInventoryHandler().clearListeners();
					inventoryIOHandler = null;
					inventoryModificationHandler = null;
					fluidHandlerInitialized = false;
					fluidHandler = null;
					energyStorageInitialized = false;
					energyStorage = null;
				});
			} else {
				upgradeHandler = NoopBackpackWrapper.INSTANCE.getUpgradeHandler();
			}
		}
		return upgradeHandler;
	}

	private int getNumberOfUpgradeSlots() {
		Optional<Integer> upgradeSlots = NBTHelper.getInt(backpack, UPGRADE_SLOTS_TAG);

		if (upgradeSlots.isPresent()) {
			return upgradeSlots.get();
		}

		int itemUpgradeSlots = ((BackpackItem) backpack.getItem()).getNumberOfUpgradeSlots();
		setNumberOfUpgradeSlots(itemUpgradeSlots);
		return itemUpgradeSlots;
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return NBTHelper.getUniqueId(backpack, CONTENTS_UUID_TAG);
	}

	private UUID getOrCreateContentsUuid() {
		Optional<UUID> contentsUuid = getContentsUuid();
		if (contentsUuid.isPresent()) {
			return contentsUuid.get();
		}
		clearDummyHandlers();
		UUID newUuid = UUID.randomUUID();
		setContentsUuid(newUuid);
		migrateBackpackContents(newUuid);
		return newUuid;
	}

	private void clearDummyHandlers() {
		if (upgradeHandler == NoopBackpackWrapper.INSTANCE.getUpgradeHandler()) {
			upgradeHandler = null;
		}
		if (settingsHandler == NoopBackpackWrapper.INSTANCE.getSettingsHandler()) {
			settingsHandler = null;
		}
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
		backpack.addTagElement(CLOTH_COLOR_TAG, IntNBT.valueOf(clothColor));
		backpack.addTagElement(BORDER_COLOR_TAG, IntNBT.valueOf(borderColor));
		backpackSaveHandler.run();
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		backpack.addTagElement(SORT_BY_TAG, StringNBT.valueOf(sortBy.getSerializedName()));
		backpackSaveHandler.run();
	}

	@Override
	public SortBy getSortBy() {
		return NBTHelper.getEnumConstant(backpack, SORT_BY_TAG, SortBy::fromName).orElse(SortBy.NAME);
	}

	@Override
	public void sort() {
		InventorySorter.sortHandler(getInventoryHandler(), getComparator(), getSettingsHandler().getTypeCategory(NoSortSettingsCategory.class).getNoSortSlots());
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
		ItemStack backpackCopy = originalWrapper.getBackpack().copy();
		backpackCopy.removeTagKey(CONTENTS_UUID_TAG);
		return backpackCopy.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapperCopy -> {
							originalWrapper.copyDataTo(wrapperCopy);
							return wrapperCopy.getBackpack();
						}
				).orElse(ItemStack.EMPTY);
	}

	@Override
	public void refreshInventoryForInputOutput() {
		inventoryIOHandler = null;
	}

	@Override
	public void setPersistent(boolean persistent) {
		getInventoryHandler().setPersistent(persistent);
		getUpgradeHandler().setPersistent(persistent);
	}

	@Override
	public void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots) {
		setNumberOfInventorySlots(numberOfInventorySlots);
		setNumberOfUpgradeSlots(numberOfUpgradeSlots);
	}

	@Override
	public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
		backpack.addTagElement(LOOT_TABLE_NAME_TAG, StringNBT.valueOf(lootTableName.toString()));
		backpack.addTagElement(LOOT_PERCENTAGE_TAG, FloatNBT.valueOf(lootPercentage));
		backpackSaveHandler.run();
	}

	@Override
	public void fillWithLoot(PlayerEntity playerEntity) {
		if (playerEntity.level.isClientSide) {
			return;
		}
		NBTHelper.getString(backpack, LOOT_TABLE_NAME_TAG).ifPresent(ltName -> fillWithLootFromTable(playerEntity, ltName));
	}

	@Override
	public void setContentsUuid(UUID backpackUuid) {
		NBTHelper.setUniqueId(backpack, CONTENTS_UUID_TAG, backpackUuid);
	}

	@Override
	public BackpackRenderInfo getRenderInfo() {
		return renderInfo;
	}

	@Override
	public void setColumnsTaken(int columnsTaken) {
		NBTHelper.setInteger(backpack, COLUMNS_TAKEN_TAG, columnsTaken);
	}

	@Override
	public int getColumnsTaken() {
		return NBTHelper.getInt(backpack, COLUMNS_TAKEN_TAG).orElse(0);
	}

	private void fillWithLootFromTable(PlayerEntity playerEntity, String lootName) {
		MinecraftServer server = playerEntity.level.getServer();
		if (server == null || !(playerEntity.level instanceof ServerWorld)) {
			return;
		}

		ResourceLocation lootTableName = new ResourceLocation(lootName);
		float lootPercentage = NBTHelper.getFloat(backpack, LOOT_PERCENTAGE_TAG).orElse(0f);

		backpack.removeTagKey(LOOT_TABLE_NAME_TAG);
		backpack.removeTagKey(LOOT_PERCENTAGE_TAG);

		ServerWorld world = (ServerWorld) playerEntity.level;

		List<ItemStack> loot = LootHelper.getLoot(lootTableName, server, world, playerEntity);
		loot = RandHelper.getNRandomElements(loot, (int) (loot.size() * lootPercentage));
		LootHelper.fillWithLoot(world.random, loot, getInventoryHandler());
	}

	private void setNumberOfUpgradeSlots(int numberOfUpgradeSlots) {
		NBTHelper.setInteger(backpack, UPGRADE_SLOTS_TAG, numberOfUpgradeSlots);
	}

	@Override
	public void refreshInventoryForUpgradeProcessing() {
		inventoryModificationHandler = null;
		fluidHandler = null;
		fluidHandlerInitialized = false;
		energyStorage = null;
		energyStorageInitialized = false;
		refreshInventoryForInputOutput();
	}

	@Override
	public void onContentsNbtUpdated() {
		handler = null;
		upgradeHandler = null;
		refreshInventoryForUpgradeProcessing();
	}
}
