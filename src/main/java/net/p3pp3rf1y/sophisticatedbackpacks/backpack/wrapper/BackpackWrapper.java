package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.Containers;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.ItemContainerContents;
import net.minecraft.world.level.Level;
import net.neoforged.fml.util.thread.SidedThreadGroups;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.access.ItemAccess;
import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.transaction.RootCommitJournal;
import net.neoforged.neoforge.transfer.transaction.Transaction;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyHandlerUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplate;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.*;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
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
import org.jspecify.annotations.Nullable;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.IntConsumer;
import java.util.function.Supplier;

public class BackpackWrapper implements IBackpackWrapper {
	public static final int DEFAULT_MAIN_COLOR = 0xFF_CC613A;
	public static final int DEFAULT_ACCENT_COLOR = 0xFF_622E1A;

	@Nullable
	private ItemStack backpack;
	private final IBackpackContentsSource contentsSource;
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
	private EnergyHandler energyStorage = null;

	@Nullable
	private RenderDataHandler renderDataHandler;
	private boolean renderDataValidationPending = false;

	private IntConsumer onSlotsChange = diff -> {
	};

	private Runnable onInventoryHandlerRefresh = () -> {
	};
	private Runnable upgradeCachesInvalidatedHandler = () -> {
	};
	private Runnable onInventoryForInputOutputHandlerRefresh = () -> {
	};
	private final boolean cacheContainedBackpackWrappers;

	public BackpackWrapper(ItemStack backpackStack) {
		this(backpackStack, true, null);
	}

	private BackpackWrapper(ItemStack backpackStack, boolean cacheContainedBackpackWrappers) {
		this(backpackStack, cacheContainedBackpackWrappers, null);
	}

	BackpackWrapper(ItemStack backpackStack, IBackpackContentsSource contentsSource) {
		this(backpackStack, true, contentsSource);
	}

	private BackpackWrapper(ItemStack backpackStack, boolean cacheContainedBackpackWrappers, @Nullable IBackpackContentsSource contentsSource) {
		this.cacheContainedBackpackWrappers = cacheContainedBackpackWrappers;
		this.contentsSource = contentsSource == null ? new BackpackStorageContentsSource() : contentsSource;
		setBackpackStack(backpackStack);
	}

	public static IBackpackWrapper fromStack(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			return Noop.INSTANCE;
		}
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) == LinkedStorageEndpointStackState.ENDPOINT) {
			Optional<IBackpackWrapper> canonicalHost = BackpackLinkedStorageResolver.resolveServerCanonicalHost(stack);
			if (canonicalHost.isPresent()) {
				return canonicalHost.get();
			}
			if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
				throw new IllegalStateException("Failed to resolve linked backpack endpoint");
			}
			return Noop.INSTANCE;
		}

		if (!stack.has(ModCoreDataComponents.STORAGE_UUID)) {
			return new BackpackWrapper(stack);
		}

		IBackpackWrapper backpackWrapper = StorageWrapperRepository.getStorageWrapper(stack, IBackpackWrapper.class, BackpackWrapper::new);
		backpackWrapper.getContentsUuid().ifPresent(uuid -> StorageWrapperRepository.registerStorageWrapper(uuid, backpackWrapper));
		return backpackWrapper;
		/*
		 * TODO try to add uuid based caching in the future UUID uuid = stack.get(ModCoreDataComponents.STORAGE_UUID); if (uuid == null) { return
		 * StorageWrapperRepository.getStorageWrapper(stack, IBackpackWrapper.class, BackpackWrapper::new); } else { return
		 * StorageWrapperRepository.getStorageWrapper(uuid, IBackpackWrapper.class, BackpackWrapper::new); }
		 */
	}

	public static Optional<IBackpackWrapper> fromExistingData(ItemStack stack) {
		if (stack.getItem() instanceof BackpackItem) {
			return StorageWrapperRepository.getExistingStorageWrapper(stack, IBackpackWrapper.class);
		}

		return Optional.empty();
	}

	public static IBackpackWrapper fromStackNoCache(ItemStack stack) {
		return new BackpackWrapper(stack, false);
	}

	public boolean shouldCacheContainedBackpackWrappers() {
		return cacheContainedBackpackWrappers;
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
	public ITrackedContentsItemResourceHandler getInventoryForUpgradeProcessing() {
		if (inventoryModificationHandler == null) {
			inventoryModificationHandler = new InventoryModificationHandler(this);
		}
		return inventoryModificationHandler.getModifiedInventoryHandler();
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		InventoryHandler inventoryHandler = handler;
		if (inventoryHandler == null) {
			inventoryHandler = new BackpackInventoryHandler(getNumberOfInventorySlots() - (getNumberOfSlotRows() * getColumnsTaken()), this,
					getBackpackContents(), () -> {
						markBackpackContentsDirty();
						getContentsUuid().ifPresent(uuid -> StorageWrapperRepository.invalidateStorageWrapperContents(uuid, this));
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

	private ContainerContents getBackpackContents() {
		return contentsSource.getContents();
	}

	ContainerContents copyContentsForLinkedStorage() {
		return getBackpackContents().copy();
	}

	private void markBackpackContentsDirty() {
		contentsSource.markDirty();
	}

	@Override
	public ITrackedContentsItemResourceHandler getInventoryForInputOutput() {
		if (inventoryIOHandler == null) {
			inventoryIOHandler = new InventoryIOHandler(this);
		}
		return inventoryIOHandler.getFilteredItemHandler();
	}

	public static ResourceHandler<ItemResource> getItemInventoryHandler(ItemAccess itemAccess) {
		return new ItemAccessBackpackInventoryHandler(itemAccess);
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
	public Optional<ResourceHandler<FluidResource>> getItemFluidHandler() {
		return getFluidHandler().map(FluidHandlerItemWrapper::new);
	}

	@Override
	public Optional<ResourceHandler<FluidResource>> getItemFluidHandler(ItemAccess itemAccess) {
		return Optional.<ResourceHandler<FluidResource>>of(new ItemAccessBackpackFluidHandler(itemAccess)).filter(handler -> handler.size() > 0);
	}

	@Override
	public Optional<EnergyHandler> getEnergyHandler() {
		if (!energyStorageInitialized) {
			EnergyHandler wrappedStorage = getUpgradeHandler().getWrappersThatImplement(EnergyHandler.class).stream().findFirst().orElse(null);

			for (IEnergyHandlerUpgradeWrapper energyStorageWrapperUpgrade : getUpgradeHandler().getWrappersThatImplement(IEnergyHandlerUpgradeWrapper.class)) {
				wrappedStorage = energyStorageWrapperUpgrade.wrapStorage(wrappedStorage);
			}

			energyStorage = wrappedStorage;
		}

		return energyStorage == null || energyStorage.getCapacityAsInt() == 0 ? Optional.empty() : Optional.of(energyStorage);
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
		cacheSlotNumbers();
		if (renderDataHandler == null) {
			Supplier<Runnable> getSaveHandler = () -> backpackSaveHandler;
			renderDataHandler = new RenderDataHandler(
					Optional.ofNullable(backpack.get(ModCoreDataComponents.RENDER_DATA)).map(RenderData::copy).orElseGet(RenderData::new), renderData -> {
						backpack.set(ModCoreDataComponents.RENDER_DATA, renderData.copy());
						getSaveHandler.get().run();
					});
		}
		renderDataValidationPending = true;
		return this;
	}

	public final void replaceBackpackStack(ItemStack backpack) {
		renderDataHandler = null;
		setBackpackStack(backpack);
	}

	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		if (settingsHandler == null) {
			if (getContentsUuid().isPresent()) {
				settingsHandler = new BackpackSettingsHandler(this, getBackpackContents().settings(), this::markBackpackContentsDirty);
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
				handler = new UpgradeHandler(getNumberOfUpgradeSlots(), this, getBackpackContents(), this::markBackpackContentsDirty, () -> {
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
					public boolean isValid(int index, ItemResource resource) {
						return super.isValid(index, resource) && (resource.isEmpty() || resource.is(ModItems.BACKPACK_UPGRADE_TAG));
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
		int resolvedNumberOfInventorySlots = contentsSource.usesCanonicalSlotNumbers() && storedNumberOfInventorySlots != null
				? storedNumberOfInventorySlots
				: Math.max(storedNumberOfInventorySlots == null ? defaultNumberOfInventorySlots : storedNumberOfInventorySlots, defaultNumberOfInventorySlots);
		numberOfInventorySlots = resolvedNumberOfInventorySlots;
		if (storedNumberOfInventorySlots == null || storedNumberOfInventorySlots < resolvedNumberOfInventorySlots) {
			backpackStack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, resolvedNumberOfInventorySlots);
		}
	}

	private void cacheNumberOfUpgradeSlots(ItemStack backpackStack, int defaultNumberOfUpgradeSlots) {
		Integer storedNumberOfUpgradeSlots = backpackStack.get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS);
		int resolvedNumberOfUpgradeSlots = contentsSource.usesCanonicalSlotNumbers() && storedNumberOfUpgradeSlots != null
				? storedNumberOfUpgradeSlots
				: Math.max(storedNumberOfUpgradeSlots == null ? defaultNumberOfUpgradeSlots : storedNumberOfUpgradeSlots, defaultNumberOfUpgradeSlots);
		numberOfUpgradeSlots = resolvedNumberOfUpgradeSlots;
		if (storedNumberOfUpgradeSlots == null || storedNumberOfUpgradeSlots < resolvedNumberOfUpgradeSlots) {
			backpackStack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, resolvedNumberOfUpgradeSlots);
		}
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return Optional.ofNullable(getBackpackStack().get(ModCoreDataComponents.STORAGE_UUID));
	}

	private UUID getOrCreateContentsUuid() {
		Optional<UUID> contentsUuid = getContentsUuid();
		if (contentsUuid.isPresent()) {
			return contentsUuid.get();
		}
		clearDummyHandlers();
		UUID newUuid = UUID.randomUUID();
		setContentsUuid(newUuid);
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
		return getBackpackStack().getOrDefault(ModCoreDataComponents.MAIN_COLOR, DEFAULT_MAIN_COLOR);
	}

	@Override
	public int getAccentColor() {
		return getBackpackStack().getOrDefault(ModCoreDataComponents.ACCENT_COLOR, DEFAULT_ACCENT_COLOR);
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		return Optional.ofNullable(getBackpackStack().get(ModCoreDataComponents.OPEN_TAB_ID));
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
		return getBackpackStack().getOrDefault(ModCoreDataComponents.SORT_BY, SortBy.NAME);
	}

	@Override
	public void sort() {
		Set<Integer> slotIndexesExcludedFromSort = new HashSet<>();
		slotIndexesExcludedFromSort.addAll(getSettingsHandler().getTypeCategory(NoSortSettingsCategory.class).getNoSortSlots());
		MemorySettingsCategory memorySettings = getSettingsHandler().getTypeCategory(MemorySettingsCategory.class);
		InventorySorter.sortHandler(getInventoryHandler(), getComparator(), slotIndexesExcludedFromSort, memorySettings.getSlotIndexes(),
				memorySettings::matchesFilter);
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
		cloneSubbackpacks(fromStack(clonedBackpack));
		return clonedBackpack;
	}

	private void cloneSubbackpacks(IStorageWrapper wrapperCloned) {
		InventoryHandler inventoryHandler = wrapperCloned.getInventoryHandler();
		InventoryHelper.iterate(inventoryHandler, (slot, stack) -> {
			if (!(stack.getItem() instanceof BackpackItem)) {
				return;
			}
			inventoryHandler.setStackInSlot(slot, cloneBackpack(fromStack(stack)));
		});
	}

	private ItemStack cloneBackpack(IBackpackWrapper originalWrapper) {
		ItemStack backpackCopy = originalWrapper.getBackpack().copy();
		backpackCopy.remove(ModCoreDataComponents.STORAGE_UUID);
		IBackpackWrapper wrapperCopy = fromStack(backpackCopy);
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
	public void setLoot(Identifier lootTableName, float lootFactor) {
		getBackpackStack().set(ModDataComponents.LOOT_TABLE, lootTableName);
		getBackpackStack().set(ModDataComponents.LOOT_FACTOR, lootFactor);
		backpackSaveHandler.run();
	}

	@Override
	public void setTemplate(Identifier templateName) {
		getBackpackStack().set(ModDataComponents.TEMPLATE_NAME, templateName);
	}

	@Override
	public void fillWithLoot(Player player) {
		Level level = player.level();
		if (level.isClientSide()) {
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
			int inserted = InventoryHelper.insert(getInventoryHandler(), ItemResource.of(stack), stack.getCount());
			if (inserted < stack.getCount()) {
				Containers.dropItemStack(level, pos.getX(), pos.getY(), pos.getZ(), stack.copyWithCount(stack.getCount() - inserted));
			}
		});
	}

	public void fillWithLoot(Level level, BlockPos pos) {
		fillWithLoot(level, pos, null);
	}

	public void fillWithLoot(Level level, BlockPos pos, @Nullable Player player) {
		Identifier lootTable = getBackpackStack().get(ModDataComponents.LOOT_TABLE);
		if (lootTable == null) {
			return;
		}
		fillWithLootFromTable(level, pos, lootTable, player);
	}

	@Override
	public void fillFromTemplate() {
		ItemStack backpack = getBackpackStack();
		Identifier templateName = backpack.get(ModDataComponents.TEMPLATE_NAME);
		if (templateName == null) {
			return;
		}

		Optional<BackpackTemplate> templateData = BackpackTemplates.getBackpackTemplate(templateName);
		if (templateData.isEmpty()) {
			return;
		}

		BackpackStorage.get().setBackpackContents(getOrCreateContentsUuid(), templateData.get().contents());
		backpack.remove(ModDataComponents.TEMPLATE_NAME);
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		ItemStack backpackStack = getBackpackStack();
		backpackStack.set(ModCoreDataComponents.STORAGE_UUID, storageUuid);
		StorageWrapperRepository.setStorageWrapper(backpackStack, this);
		StorageWrapperRepository.registerStorageWrapper(storageUuid, this);
		/*
		 * TODO add in the future StorageWrapperRepository.migrateToUuid(this, backpack, storageUuid);
		 */
	}

	@Override
	public void removeContentsUuid() {
		if (contentsSource.usesLegacyBackpackDataMigration()) {
			getContentsUuid().ifPresent(BackpackStorage.get()::removeBackpackContents);
		}
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
	public RenderDataHandler getRenderDataHandler() {
		return renderDataHandler;
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
		return getBackpackStack().getOrDefault(ModDataComponents.COLUMNS_TAKEN, 0);
	}

	private void fillWithLootFromTable(Level level, BlockPos pos, Identifier lootTable, @Nullable Player player) {
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
		if (renderDataValidationPending && !level.isClientSide()) {
			getRenderDataHandler().validate(this, level);
			renderDataValidationPending = false;
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
	public void onContentsUpdated() {
		handler = null;
		upgradeHandler = null;
		settingsHandler = null;
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

	private static class FluidHandlerItemWrapper implements ResourceHandler<FluidResource> {
		private final ResourceHandler<FluidResource> delegate;

		public FluidHandlerItemWrapper(ResourceHandler<FluidResource> delegate) {
			this.delegate = delegate;
		}

		@Override
		public int size() {
			return delegate.size();
		}

		@Override
		public FluidResource getResource(int index) {
			return delegate.getResource(index);
		}

		@Override
		public long getAmountAsLong(int index) {
			return delegate.getAmountAsLong(index);
		}

		@Override
		public long getCapacityAsLong(int index, FluidResource resource) {
			return delegate.getCapacityAsLong(index, resource);
		}

		@Override
		public boolean isValid(int index, FluidResource resource) {
			return delegate.isValid(index, resource);
		}

		@Override
		public int insert(int index, FluidResource resource, int amount, TransactionContext tx) {
			return delegate.insert(index, resource, amount, tx);
		}

		@Override
		public int extract(int index, FluidResource resource, int amount, TransactionContext tx) {
			return delegate.extract(index, resource, amount, tx);
		}
	}

	private static class ItemAccessBackpackFluidHandler implements ResourceHandler<FluidResource> {
		private final ItemAccess itemAccess;

		private ItemAccessBackpackFluidHandler(ItemAccess itemAccess) {
			this.itemAccess = itemAccess;
		}

		private record MutableBackpackDelegate(ItemStack backpackStack, ResourceHandler<FluidResource> delegate) {
		}

		private Optional<ResourceHandler<FluidResource>> getDelegate() {
			ItemStack backpackStack = itemAccess.getResource().toStack(itemAccess.getAmount());
			if (backpackStack.isEmpty()) {
				return Optional.empty();
			}
			return new BackpackWrapper(backpackStack).getFluidHandler().map(FluidHandlerItemWrapper::new);
		}

		private Optional<MutableBackpackDelegate> getWritableDelegate() {
			ItemStack backpackStack = itemAccess.getResource().toStack(itemAccess.getAmount());
			if (backpackStack.isEmpty()) {
				return Optional.empty();
			}

			BackpackWrapper wrapper = new BackpackWrapper(backpackStack);
			return wrapper.getFluidHandler().map(FluidHandlerItemWrapper::new).map(delegate -> new MutableBackpackDelegate(backpackStack, delegate));
		}

		@Override
		public int size() {
			return getDelegate().map(ResourceHandler::size).orElse(0);
		}

		@Override
		public FluidResource getResource(int index) {
			return getDelegate().map(delegate -> delegate.getResource(index)).orElse(FluidResource.EMPTY);
		}

		@Override
		public long getAmountAsLong(int index) {
			return getDelegate().map(delegate -> delegate.getAmountAsLong(index)).orElse(0L);
		}

		@Override
		public long getCapacityAsLong(int index, FluidResource resource) {
			return getDelegate().map(delegate -> delegate.getCapacityAsLong(index, resource)).orElse(0L);
		}

		@Override
		public boolean isValid(int index, FluidResource resource) {
			return getDelegate().map(delegate -> delegate.isValid(index, resource)).orElse(false);
		}

		@Override
		public int insert(int index, FluidResource resource, int amount, TransactionContext tx) {
			return getWritableDelegate().map(mutableDelegate -> {
				int inserted = mutableDelegate.delegate().insert(index, resource, amount, tx);
				if (inserted > 0) {
					itemAccess.exchange(ItemResource.of(mutableDelegate.backpackStack()), itemAccess.getAmount(), tx);
				}
				return inserted;
			}).orElse(0);
		}

		@Override
		public int extract(int index, FluidResource resource, int amount, TransactionContext tx) {
			return getWritableDelegate().map(mutableDelegate -> {
				int extracted = mutableDelegate.delegate().extract(index, resource, amount, tx);
				if (extracted > 0) {
					itemAccess.exchange(ItemResource.of(mutableDelegate.backpackStack()), itemAccess.getAmount(), tx);
				}
				return extracted;
			}).orElse(0);
		}
	}

	private static class ItemAccessBackpackInventoryHandler implements ResourceHandler<ItemResource> {
		private final ItemAccess itemAccess;
		private final RootCommitJournal exchangeBackpackStackJournal = new RootCommitJournal(this::exchangeBackpackStack);
		@Nullable
		private ItemStack backpackStackToExchange = null;
		@Nullable
		private ResourceHandler<ItemResource> delegate = null;
		@Nullable
		private ItemStack delegateBackpackStack = null;

		private ItemAccessBackpackInventoryHandler(ItemAccess itemAccess) {
			this.itemAccess = itemAccess;
		}

		private Optional<ResourceHandler<ItemResource>> getDelegate() {
			ItemStack currentBackpackStack = getBackpackStack();
			if (currentBackpackStack.isEmpty()) {
				delegate = null;
				delegateBackpackStack = null;
				return Optional.empty();
			}

			if (delegate != null && delegateBackpackStack != null && ItemStack.isSameItemSameComponents(currentBackpackStack, delegateBackpackStack)) {
				return Optional.of(delegate);
			}

			delegateBackpackStack = currentBackpackStack;
			delegate = new BackpackWrapper(delegateBackpackStack, false).getInventoryForInputOutput();
			return Optional.of(delegate);
		}

		@Override
		public int size() {
			return getDelegate().map(ResourceHandler::size).orElse(0);
		}

		@Override
		public ItemResource getResource(int index) {
			return getDelegate().map(delegate -> delegate.getResource(index)).orElse(ItemResource.EMPTY);
		}

		@Override
		public long getAmountAsLong(int index) {
			return getDelegate().map(delegate -> delegate.getAmountAsLong(index)).orElse(0L);
		}

		@Override
		public long getCapacityAsLong(int index, ItemResource resource) {
			return getDelegate().map(delegate -> delegate.getCapacityAsLong(index, resource)).orElse(0L);
		}

		@Override
		public boolean isValid(int index, ItemResource resource) {
			return getDelegate().map(delegate -> delegate.isValid(index, resource)).orElse(false);
		}

		@Override
		public int insert(int index, ItemResource resource, int amount, TransactionContext tx) {
			int inserted = getDelegate().map(delegate -> delegate.insert(index, resource, amount, tx)).orElse(0);
			if (inserted > 0) {
				exchangeBackpackStack(tx);
			}
			return inserted;
		}

		@Override
		public int extract(int index, ItemResource resource, int amount, TransactionContext tx) {
			int extracted = getDelegate().map(delegate -> delegate.extract(index, resource, amount, tx)).orElse(0);
			if (extracted > 0) {
				exchangeBackpackStack(tx);
			}
			return extracted;
		}

		private ItemStack getBackpackStack() {
			int accessAmount = itemAccess.getAmount();
			ItemStack backpackStack = itemAccess.getResource().toStack(accessAmount);
			return accessAmount > 0 && backpackStack.has(ModCoreDataComponents.STORAGE_UUID) ? backpackStack : ItemStack.EMPTY;
		}

		private void exchangeBackpackStack(TransactionContext tx) {
			backpackStackToExchange = delegateBackpackStack;
			exchangeBackpackStackJournal.updateSnapshots(tx);
		}

		private void exchangeBackpackStack() {
			ItemStack backpackStack = backpackStackToExchange;
			backpackStackToExchange = null;
			if (backpackStack == null || backpackStack.isEmpty()) {
				return;
			}

			try (Transaction tx = Transaction.openRoot()) {
				itemAccess.exchange(ItemResource.of(backpackStack), itemAccess.getAmount(), tx);
				tx.commit();
			}
		}
	}

	private class BackpackStorageContentsSource implements IBackpackContentsSource {
		@Override
		public ContainerContents getContents() {
			return BackpackStorage.get().getOrCreateBackpackContents(getOrCreateContentsUuid());
		}

		@Override
		public void setContents(ContainerContents contents) {
			BackpackStorage.get().setBackpackContents(getOrCreateContentsUuid(), contents);
		}

		@Override
		public void markDirty() {
			BackpackStorage.get().setDirty();
		}

		@Override
		public Optional<UUID> getContentsUuid() {
			return BackpackWrapper.this.getContentsUuid();
		}

		@Override
		public boolean usesLegacyBackpackDataMigration() {
			return true;
		}
	}
}
