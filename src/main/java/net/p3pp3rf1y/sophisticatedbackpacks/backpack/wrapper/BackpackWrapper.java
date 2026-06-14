package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Containers;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.ItemContainerContents;
import net.minecraft.world.level.Level;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.energy.IEnergyStorage;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.IFluidHandlerItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyStorageUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.*;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.InventorySorter;
import net.p3pp3rf1y.sophisticatedcore.util.LootHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RandHelper;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.function.Supplier;

public class BackpackWrapper implements IBackpackWrapper {
	public static final int DEFAULT_MAIN_COLOR = 0xFF_CC613A;
	public static final int DEFAULT_ACCENT_COLOR = 0xFF_622E1A;

	@Nullable
	private ItemStack backpack;
	private int numberOfInventorySlots = -1;
	private int numberOfUpgradeSlots = -1;
	private Runnable backpackSaveHandler = () -> {
	};
	private Runnable inventorySlotChangeHandler = () -> {
	};

	@Nullable
	private InventoryHandler handler = null;
	@Nullable
	private UpgradeHandler upgradeHandler = null;
	@Nullable
	private InventoryIOHandler inventoryIOHandler = null;
	@Nullable
	private InventoryModificationHandler inventoryModificationHandler = null;
	@Nullable
	private BackpackSettingsHandler settingsHandler = null;
	private boolean fluidHandlerInitialized = false;
	@Nullable
	private IStorageFluidHandler fluidHandler = null;
	private boolean energyStorageInitialized = false;
	@Nullable
	private IEnergyStorage energyStorage = null;

	@Nullable
	private BackpackRenderInfo renderInfo;
	private boolean renderInfoValidationPending = false;

	private IntConsumer onSlotsChange = diff -> {
	};

	private Runnable onInventoryHandlerRefresh = () -> {
	};
	private Runnable upgradeCachesInvalidatedHandler = () -> {
	};
	private Runnable onInventoryForInputOutputHandlerRefresh = () -> {
	};

	public BackpackWrapper(ItemStack backpackStack) {
		setBackpackStack(backpackStack);
	}

	public static IBackpackWrapper fromStack(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			return Noop.INSTANCE;
		}

		if (!stack.has(ModCoreDataComponents.STORAGE_UUID)) {
			IBackpackWrapper wrapper = new BackpackWrapper(stack);
			if (stack.has(ModCoreDataComponents.STORAGE_UUID)) {
				StorageWrapperRepository.setStorageWrapper(stack, wrapper);
			}
			return wrapper;
		}

		return StorageWrapperRepository.getStorageWrapper(stack, IBackpackWrapper.class, BackpackWrapper::new);
		/* TODO try to add uuid based caching in the future
		UUID uuid = stack.get(ModCoreDataComponents.STORAGE_UUID);
		if (uuid == null) {
			return StorageWrapperRepository.getStorageWrapper(stack, IBackpackWrapper.class, BackpackWrapper::new);
		} else {
			return StorageWrapperRepository.getStorageWrapper(uuid, IBackpackWrapper.class, BackpackWrapper::new);
		}
*/
	}

	public static Optional<IBackpackWrapper> fromExistingData(ItemStack stack) {
		if (stack.getItem() instanceof BackpackItem) {
			return StorageWrapperRepository.getExistingStorageWrapper(stack, IBackpackWrapper.class);
		}

		return Optional.empty();
	}

	@Override
	public void setContentsChangeHandler(Runnable contentsChangeHandler) {
		backpackSaveHandler = contentsChangeHandler;
		refreshInventoryForUpgradeProcessing();
	}

	@Override
	public void setInventorySlotChangeHandler(Runnable slotChangeHandler) {
		inventorySlotChangeHandler = slotChangeHandler;
	}

	@Override
	public ITrackedContentsItemHandler getInventoryForUpgradeProcessing() {
		if (inventoryModificationHandler == null) {
			inventoryModificationHandler = new InventoryModificationHandler(this);
		}
		return inventoryModificationHandler.getModifiedInventoryHandler();
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		InventoryHandler inventoryHandler = handler;
		if (inventoryHandler == null) {
			inventoryHandler = new BackpackInventoryHandler(getNumberOfInventorySlots() - (getNumberOfSlotRows() * getColumnsTaken()),
					this, getBackpackContentsNbt(), () -> {
				markBackpackContentsDirty();
				if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
					inventorySlotChangeHandler.run();
				}
			}, StackUpgradeItem.getInventorySlotLimit(this));
			handler = inventoryHandler;
			inventoryHandler.addListener(getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class)::itemChanged);
			attachInventorySlotBlockers();
		}
		return inventoryHandler;
	}

	private int getNumberOfInventorySlots() {
		if (numberOfInventorySlots < 0) {
			cacheSlotNumbers();
		}
		return numberOfInventorySlots;
	}

	@Override
	public int getNumberOfSlotRows() {
		int itemInventorySlots = getNumberOfInventorySlots();
		return (int) Math.ceil(itemInventorySlots <= 81 ? (double) itemInventorySlots / 9 : (double) itemInventorySlots / 12);
	}

	private void setNumberOfInventorySlots(int itemInventorySlots) {
		numberOfInventorySlots = itemInventorySlots;
		getBackpackStack().set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, itemInventorySlots);
	}

	private CompoundTag getBackpackContentsNbt() {
		return BackpackStorage.get().getOrCreateBackpackContents(getOrCreateContentsUuid());
	}

	private void markBackpackContentsDirty() {
		BackpackStorage.get().setDirty();
	}

	@Override
	public ITrackedContentsItemHandler getInventoryForInputOutput() {
		if (inventoryIOHandler == null) {
			inventoryIOHandler = new InventoryIOHandler(this);
		}
		return inventoryIOHandler.getFilteredItemHandler();
	}

	@Override
	public Optional<IStorageFluidHandler> getFluidHandler() {
		if (!fluidHandlerInitialized) {
			IStorageFluidHandler wrappedHandler = getUpgradeHandler().getTypeWrappers(TankUpgradeItem.TYPE).isEmpty() ? null : new BackpackFluidHandler(this);
			List<IFluidHandlerWrapperUpgrade> fluidHandlerWrapperUpgrades = getUpgradeHandler().getWrappersThatImplement(IFluidHandlerWrapperUpgrade.class);

			for (IFluidHandlerWrapperUpgrade fluidHandlerWrapperUpgrade : fluidHandlerWrapperUpgrades) {
				wrappedHandler = fluidHandlerWrapperUpgrade.wrapHandler(wrappedHandler, getBackpackStack());
			}

			fluidHandler = wrappedHandler;
			fluidHandlerInitialized = true;
		}

		return Optional.ofNullable(fluidHandler);
	}

	@Override
	public Optional<IFluidHandlerItem> getItemFluidHandler() {
		return getFluidHandler().map(fh -> new FluidHandlerItemWrapper(getBackpackStack(), fh));
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

		return energyStorage == null || energyStorage.getMaxEnergyStored() == 0 ? Optional.empty() : Optional.of(energyStorage);
	}

	@Override
	public void copyDataTo(IStorageWrapper otherStorageWrapper) {
		getContentsUuid().ifPresent(originalUuid -> {
			getInventoryHandler().copyStacksTo(otherStorageWrapper.getInventoryHandler());
			getUpgradeHandler().copyTo(otherStorageWrapper.getUpgradeHandler());
			getSettingsHandler().copyTo(otherStorageWrapper.getSettingsHandler());
		});
	}

	@Override
	public IBackpackWrapper setBackpackStack(ItemStack backpack) {
		this.backpack = backpack;
		LegacyBackpackDataMigration.normalizeLegacyData(backpack);
		cacheSlotNumbers();
		if (renderInfo == null) {
			Supplier<Runnable> getSaveHandler = () -> backpackSaveHandler;
			renderInfo = new BackpackRenderInfo(backpack, getSaveHandler);
		}
		renderInfoValidationPending = true;
		return this;
	}

	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		if (settingsHandler == null) {
			if (getContentsUuid().isPresent()) {
				settingsHandler = new BackpackSettingsHandler(this, getBackpackContentsNbt(), this::markBackpackContentsDirty);
			} else {
				settingsHandler = Noop.INSTANCE.getSettingsHandler();
			}
		}
		return settingsHandler;
	}

	@Override
	public UpgradeHandler getUpgradeHandler() {
		UpgradeHandler handler = upgradeHandler;
		if (handler == null) {
			if (getContentsUuid().isPresent()) {
				handler = new UpgradeHandler(getNumberOfUpgradeSlots(), this, getBackpackContentsNbt(), this::markBackpackContentsDirty, () -> {
					InventoryHandler inventoryHandler = this.handler;
					if (inventoryHandler != null) {
						inventoryHandler.clearListeners();
						inventoryHandler.setBaseSlotLimit(StackUpgradeItem.getInventorySlotLimit(this));
					}
					inventoryHandler = getInventoryHandler();
					inventoryHandler.clearListeners();
					inventoryHandler.addListener(getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class)::itemChanged);
					inventoryIOHandler = null;
					inventoryModificationHandler = null;
					fluidHandlerInitialized = false;
					fluidHandler = null;
					energyStorageInitialized = false;
					energyStorage = null;
					upgradeCachesInvalidatedHandler.run();
				}) {
					@Override
					public boolean isItemValid(int slot, ItemStack stack) {
						return super.isItemValid(slot, stack) && (stack.isEmpty() || stack.is(ModItems.BACKPACK_UPGRADE_TAG));
					}
				};
			} else {
				handler = Noop.INSTANCE.getUpgradeHandler();
			}
			upgradeHandler = handler;
		}
		return handler;
	}

	@Override
	public void setUpgradeCachesInvalidatedHandler(Runnable handler) {
		upgradeCachesInvalidatedHandler = handler;
	}

	private int getNumberOfUpgradeSlots() {
		if (numberOfUpgradeSlots < 0) {
			cacheSlotNumbers();
		}
		return numberOfUpgradeSlots;
	}

	private void cacheSlotNumbers() {
		ItemStack backpackStack = getBackpackStack();
		BackpackItem backpackItem = (BackpackItem) backpackStack.getItem();
		cacheNumberOfInventorySlots(backpackStack, backpackItem.getNumberOfSlots());
		cacheNumberOfUpgradeSlots(backpackStack, backpackItem.getNumberOfUpgradeSlots());
	}

	private void cacheNumberOfInventorySlots(ItemStack backpackStack, int defaultNumberOfInventorySlots) {
		Integer storedNumberOfInventorySlots = backpackStack.get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS);
		int resolvedNumberOfInventorySlots = Math.max(storedNumberOfInventorySlots == null ? defaultNumberOfInventorySlots : storedNumberOfInventorySlots, defaultNumberOfInventorySlots);
		numberOfInventorySlots = resolvedNumberOfInventorySlots;
		if (storedNumberOfInventorySlots == null || storedNumberOfInventorySlots < resolvedNumberOfInventorySlots) {
			backpackStack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, resolvedNumberOfInventorySlots);
		}
	}

	private void cacheNumberOfUpgradeSlots(ItemStack backpackStack, int defaultNumberOfUpgradeSlots) {
		Integer storedNumberOfUpgradeSlots = backpackStack.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS);
		int resolvedNumberOfUpgradeSlots = Math.max(storedNumberOfUpgradeSlots == null ? defaultNumberOfUpgradeSlots : storedNumberOfUpgradeSlots, defaultNumberOfUpgradeSlots);
		numberOfUpgradeSlots = resolvedNumberOfUpgradeSlots;
		if (storedNumberOfUpgradeSlots == null || storedNumberOfUpgradeSlots < resolvedNumberOfUpgradeSlots) {
			backpackStack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, resolvedNumberOfUpgradeSlots);
		}
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		ItemStack backpackStack = getBackpackStack();
		UUID contentsUuid = backpackStack.get(ModCoreDataComponents.STORAGE_UUID);
		if (contentsUuid != null) {
			return Optional.of(contentsUuid);
		}

		return LegacyBackpackDataMigration.getContentsUuid(backpackStack).map(legacyContentsUuid -> {
			setContentsUuid(legacyContentsUuid);
			return legacyContentsUuid;
		});
	}

	private UUID getOrCreateContentsUuid() {
		Optional<UUID> contentsUuid = getContentsUuid();
		if (contentsUuid.isPresent()) {
			return contentsUuid.get();
		}
		clearDummyHandlers();
		UUID newUuid = UUID.randomUUID();
		setContentsUuid(newUuid);
		LegacyBackpackDataMigration.migrateBackpackContents(getBackpackStack(), newUuid);
		return newUuid;
	}

	private void clearDummyHandlers() {
		if (upgradeHandler == Noop.INSTANCE.getUpgradeHandler()) {
			upgradeHandler = null;
		}
		if (settingsHandler == Noop.INSTANCE.getSettingsHandler()) {
			settingsHandler = null;
		}
	}

	@Override
	public int getMainColor() {
		return BackpackItem.getMainColor(getBackpackStack());
	}

	@Override
	public int getAccentColor() {
		return BackpackItem.getAccentColor(getBackpackStack());
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		ItemStack backpackStack = getBackpackStack();
		Integer openTabId = backpackStack.get(ModCoreDataComponents.OPEN_TAB_ID);
		if (openTabId != null) {
			return Optional.of(openTabId);
		}

		return LegacyBackpackDataMigration.getOpenTabId(backpackStack).map(legacyOpenTabId -> {
			backpackStack.set(ModCoreDataComponents.OPEN_TAB_ID, legacyOpenTabId);
			return legacyOpenTabId;
		});
	}

	@Override
	public void setOpenTabId(int openTabId) {
		getBackpackStack().set(ModCoreDataComponents.OPEN_TAB_ID, openTabId);
		backpackSaveHandler.run();
	}

	@Override
	public void removeOpenTabId() {
		getBackpackStack().remove(ModCoreDataComponents.OPEN_TAB_ID);
		backpackSaveHandler.run();
	}

	@Override
	public void setColors(int mainColor, int accentColor) {
		ItemStack backpackStack = getBackpackStack();
		BackpackItem.setColors(backpackStack, mainColor, accentColor);
		backpackSaveHandler.run();
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		getBackpackStack().set(ModCoreDataComponents.SORT_BY, sortBy);
		backpackSaveHandler.run();
	}

	@Override
	public SortBy getSortBy() {
		ItemStack backpackStack = getBackpackStack();
		SortBy sortBy = backpackStack.get(ModCoreDataComponents.SORT_BY);
		if (sortBy != null) {
			return sortBy;
		}

		return LegacyBackpackDataMigration.getSortBy(backpackStack).map(legacySortBy -> {
			backpackStack.set(ModCoreDataComponents.SORT_BY, legacySortBy);
			return legacySortBy;
		}).orElse(SortBy.NAME);
	}

	@Override
	public void sort() {
		Set<Integer> slotIndexesExcludedFromSort = new HashSet<>();
		slotIndexesExcludedFromSort.addAll(getSettingsHandler().getTypeCategory(NoSortSettingsCategory.class).getNoSortSlots());
		slotIndexesExcludedFromSort.addAll(getSettingsHandler().getTypeCategory(MemorySettingsCategory.class).getSlotIndexes());
		InventorySorter.sortHandler(getInventoryHandler(), getComparator(), slotIndexesExcludedFromSort);
	}

	private Comparator<Map.Entry<ItemStackKey, Integer>> getComparator() {
		return switch (getSortBy()) {
			case COUNT -> InventorySorter.BY_COUNT;
			case TAGS -> InventorySorter.BY_TAGS;
			case NAME -> InventorySorter.BY_NAME;
			case MOD -> InventorySorter.BY_MOD;
		};
	}

	public ItemStack getBackpack() {
		return getBackpackStack();
	}

	@Override
	public ItemStack cloneBackpack() {
		ItemStack clonedBackpack = cloneBackpack(this);
		cloneSubbackpacks(BackpackWrapper.fromStack(clonedBackpack));
		return clonedBackpack;
	}

	private void cloneSubbackpacks(IStorageWrapper wrapperCloned) {
		InventoryHandler inventoryHandler = wrapperCloned.getInventoryHandler();
		InventoryHelper.iterate(inventoryHandler, (slot, stack) -> {
			if (!(stack.getItem() instanceof BackpackItem)) {
				return;
			}
			inventoryHandler.setStackInSlot(slot, cloneBackpack(BackpackWrapper.fromStack(stack)));
		});
	}

	private ItemStack cloneBackpack(IBackpackWrapper originalWrapper) {
		ItemStack backpackCopy = originalWrapper.getBackpack().copy();
		backpackCopy.remove(ModCoreDataComponents.STORAGE_UUID);
		IBackpackWrapper wrapperCopy = BackpackWrapper.fromStack(backpackCopy);
		originalWrapper.copyDataTo(wrapperCopy);
		return wrapperCopy.getBackpack();
	}

	@Override
	public void refreshInventoryForInputOutput() {
		inventoryIOHandler = null;
		upgradeCachesInvalidatedHandler.run();
		onInventoryForInputOutputHandlerRefresh.run();
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
	public void setLoot(ResourceLocation lootTableName, float lootFactor) {
		getBackpackStack().set(ModDataComponents.LOOT_TABLE, lootTableName);
		getBackpackStack().set(ModDataComponents.LOOT_FACTOR, lootFactor);
		backpackSaveHandler.run();
	}

	@Override
	public void setTemplate(ResourceLocation templateName) {
		getBackpackStack().set(ModDataComponents.TEMPLATE_NAME, templateName);
	}

	@Override
	public void fillWithLoot(Player player) {
		Level level = player.level();
		if (level.isClientSide) {
			return;
		}
		fillFromTemplate();
		fillWithLoot(level, player.blockPosition(), player);
		fillWithExtraItems(stack -> InventoryHelper.insertOrDropItem(player, stack, getInventoryHandler()));
	}

	private void fillWithExtraItems(Consumer<ItemStack> insertOrDropItem) {
		ItemStack backpack = getBackpackStack();
		if (!backpack.has(DataComponents.CONTAINER)) {
			return;
		}

		ItemContainerContents containerItems = backpack.getOrDefault(DataComponents.CONTAINER, ItemContainerContents.EMPTY);
		for (int slot = 0; slot < containerItems.getSlots(); slot++) {
			ItemStack stack = containerItems.getStackInSlot(slot);
			if (stack.isEmpty()) {
				continue;
			}
			insertOrDropItem.accept(stack);
		}
		backpack.remove(DataComponents.CONTAINER);
	}

	@Override
	public void fillWithLootAndExtraItems(Level level, BlockPos pos) {
		fillWithLoot(level, pos);
		fillWithExtraItems(stack -> {
			ItemStack remaining = InventoryHelper.insertIntoInventory(stack, getInventoryHandler(), false);
			if (!remaining.isEmpty()) {
				Containers.dropItemStack(level, pos.getX(), pos.getY(), pos.getZ(), remaining);
			}
		});
	}

	public void fillWithLoot(Level level, BlockPos pos) {
		fillWithLoot(level, pos, null);
	}

	public void fillWithLoot(Level level, BlockPos pos, @Nullable Player player) {
		ItemStack backpackStack = getBackpackStack();
		ResourceLocation lootTable = backpackStack.get(ModDataComponents.LOOT_TABLE);
		if (lootTable == null) {
			lootTable = LegacyBackpackDataMigration.getLootTableName(backpackStack).orElse(null);
			if (lootTable != null) {
				backpackStack.set(ModDataComponents.LOOT_TABLE, lootTable);
				LegacyBackpackDataMigration.getLootPercentage(backpackStack).ifPresent(lootFactor -> backpackStack.set(ModDataComponents.LOOT_FACTOR, lootFactor));
			}
		}
		if (lootTable == null) {
			return;
		}
		fillWithLootFromTable(level, pos, lootTable, player);
	}

	@Override
	public void fillFromTemplate() {
		ItemStack backpack = getBackpackStack();
		ResourceLocation templateName = backpack.get(ModDataComponents.TEMPLATE_NAME);
		if (templateName == null) {
			templateName = LegacyBackpackDataMigration.getTemplateName(backpack).orElse(null);
			if (templateName != null) {
				backpack.set(ModDataComponents.TEMPLATE_NAME, templateName);
			}
		}
		if (templateName == null) {
			return;
		}

		Optional<CompoundTag> templateData = BackpackTemplates.getBackpackTemplate(templateName);
		if (templateData.isEmpty()) {
			return;
		}

		CompoundTag backpackContent = templateData.get().getCompound("backpackContents").copy();
		BackpackStorage.get().setBackpackContents(getOrCreateContentsUuid(), backpackContent);
		backpack.remove(ModDataComponents.TEMPLATE_NAME);
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		ItemStack backpackStack = getBackpackStack();
		backpackStack.set(ModCoreDataComponents.STORAGE_UUID, storageUuid);
		StorageWrapperRepository.setStorageWrapper(backpackStack, this);
/* TODO add in the future
		StorageWrapperRepository.migrateToUuid(this, backpack, storageUuid);
*/
	}

	@Override
	public void removeContentsUuid() {
		getContentsUuid().ifPresent(BackpackStorage.get()::removeBackpackContents);
		removeContentsUUIDTag();
	}

	@Override
	public void removeContentsUUIDTag() {
		getBackpackStack().remove(ModCoreDataComponents.STORAGE_UUID);
	}

	private ItemStack getBackpackStack() {
		if (backpack == null) {
			throw new IllegalStateException("Backpack stack not set");
		}
		return backpack;
	}

	@Override
	public BackpackRenderInfo getRenderInfo() {
		return renderInfo;
	}

	@Override
	public void setColumnsTaken(int columnsTaken, boolean hasChanged) {
		int originalColumnsTaken = getColumnsTaken();
		getBackpackStack().set(ModDataComponents.COLUMNS_TAKEN, columnsTaken);
		if (hasChanged) {
			int diff = (columnsTaken - originalColumnsTaken) * getNumberOfSlotRows();
			onSlotsChange.accept(diff);
		}
		backpackSaveHandler.run();
	}

	@Override
	public void registerOnSlotsChangeListener(IntConsumer onSlotsChange) {
		this.onSlotsChange = onSlotsChange;
	}

	@Override
	public void unregisterOnSlotsChangeListener() {
		onSlotsChange = diff -> {
		};
	}

	@Override
	public int getColumnsTaken() {
		ItemStack backpackStack = getBackpackStack();
		Integer columnsTaken = backpackStack.get(ModDataComponents.COLUMNS_TAKEN);
		if (columnsTaken != null) {
			return columnsTaken;
		}

		return LegacyBackpackDataMigration.getColumnsTaken(backpackStack).map(legacyColumnsTaken -> {
			backpackStack.set(ModDataComponents.COLUMNS_TAKEN, legacyColumnsTaken);
			return legacyColumnsTaken;
		}).orElse(0);
	}

	private void fillWithLootFromTable(Level level, BlockPos pos, ResourceLocation lootTable, @Nullable Player player) {
		MinecraftServer server = level.getServer();
		if (server == null || !(level instanceof ServerLevel serverLevel)) {
			return;
		}

		float lootFactor = getBackpackStack().getOrDefault(ModDataComponents.LOOT_FACTOR, 0f);

		getBackpackStack().remove(ModDataComponents.LOOT_TABLE);
		getBackpackStack().remove(ModDataComponents.LOOT_FACTOR);

		List<ItemStack> loot = new ArrayList<>();
		while (lootFactor > 0) {
			List<ItemStack> generatedLoot = LootHelper.getLoot(lootTable, server, serverLevel, pos, player);
			generatedLoot.removeIf(stack -> stack.getItem() instanceof BackpackItem);
			loot.addAll(RandHelper.getNRandomElements(generatedLoot, (int) (generatedLoot.size() * (lootFactor > 1 ? 1 : lootFactor))));
			lootFactor--;
		}
		LootHelper.fillWithLoot(serverLevel.random, loot, getInventoryHandler());
	}

	private void setNumberOfUpgradeSlots(int numberOfUpgradeSlots) {
		this.numberOfUpgradeSlots = numberOfUpgradeSlots;
		getBackpackStack().set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, numberOfUpgradeSlots);
	}

	@Override
	public void onInit(Level level) {
		IBackpackWrapper.super.onInit(level);
		if (renderInfoValidationPending && !level.isClientSide()) {
			getRenderInfo().validate(this, level);
			renderInfoValidationPending = false;
		}
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
		onInventoryHandlerRefresh.run();
	}

	@Override
	public void registerOnInventoryHandlerRefreshListener(Runnable onInventoryHandlerRefresh) {
		this.onInventoryHandlerRefresh = onInventoryHandlerRefresh;
	}

	@Override
	public void registerOnInventoryInputOutputHandlerRefreshListener(Runnable onInventoryForInputOutputHandlerRefresh) {
		this.onInventoryForInputOutputHandlerRefresh = onInventoryForInputOutputHandlerRefresh;
	}

	@Override
	public void unregisterOnInventoryHandlerRefreshListener() {
		onInventoryHandlerRefresh = () -> {
		};
	}

	@Override
	public ItemStack getWrappedStorageStack() {
		return getBackpack();
	}

	@Override
	public String getStorageType() {
		return "backpack";
	}

	@Override
	public Component getDisplayName() {
		return getBackpack().getHoverName();
	}

	private static class FluidHandlerItemWrapper implements IFluidHandlerItem {
		private final IFluidHandler delegate;
		private final ItemStack container;

		public FluidHandlerItemWrapper(ItemStack container, IFluidHandler delegate) {
			this.container = container;
			this.delegate = delegate;
		}


		@Override
		public ItemStack getContainer() {
			return container;
		}

		@Override
		public int getTanks() {
			return delegate.getTanks();
		}

		@Override
		public FluidStack getFluidInTank(int tank) {
			return delegate.getFluidInTank(tank);
		}

		@Override
		public int getTankCapacity(int tank) {
			return delegate.getTankCapacity(tank);
		}

		@Override
		public boolean isFluidValid(int tank, FluidStack stack) {
			return delegate.isFluidValid(tank, stack);
		}

		@Override
		public int fill(FluidStack resource, FluidAction action) {
			return delegate.fill(resource, action);
		}

		@Override
		public FluidStack drain(FluidStack resource, FluidAction action) {
			return delegate.drain(resource, action);
		}

		@Override
		public FluidStack drain(int maxDrain, FluidAction action) {
			return delegate.drain(maxDrain, action);
		}
	}
}
