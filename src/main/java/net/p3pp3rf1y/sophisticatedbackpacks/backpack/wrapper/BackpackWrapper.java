package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.core.component.DataComponents;
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
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.access.ItemAccess;
import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.item.ItemResource;
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

	public BackpackWrapper(ItemStack backpackStack) {
		setBackpackStack(backpackStack);
	}

	public static IBackpackWrapper fromStack(ItemStack stack) {
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
	public ITrackedContentsItemResourceHandler getInventoryForUpgradeProcessing() {
		if (inventoryModificationHandler == null) {
			inventoryModificationHandler = new InventoryModificationHandler(this);
		}
		return inventoryModificationHandler.getModifiedInventoryHandler();
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		if (handler == null) {
			handler = new BackpackInventoryHandler(getNumberOfInventorySlots() - (getNumberOfSlotRows() * getColumnsTaken()),
					this, getBackpackContents(), () -> {
				markBackpackContentsDirty();
				if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
					inventorySlotChangeHandler.run();
				}
			}, StackUpgradeItem.getInventorySlotLimit(this));
			handler.addListener(getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class)::itemChanged);
		}
		return handler;
	}

	private int getNumberOfInventorySlots() {
		Integer inventorySlots = getBackpackStack().get(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS);

		if (inventorySlots != null) {
			return inventorySlots;
		}

		int itemInventorySlots = ((BackpackItem) getBackpackStack().getItem()).getNumberOfSlots();
		setNumberOfInventorySlots(itemInventorySlots);
		return itemInventorySlots;
	}

	@Override
	public int getNumberOfSlotRows() {
		int itemInventorySlots = getNumberOfInventorySlots();
		return (int) Math.ceil(itemInventorySlots <= 81 ? (double) itemInventorySlots / 9 : (double) itemInventorySlots / 12);
	}

	private void setNumberOfInventorySlots(int itemInventorySlots) {
		getBackpackStack().set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, itemInventorySlots);
	}

	private ContainerContents getBackpackContents() {
		return BackpackStorage.get().getOrCreateBackpackContents(getOrCreateContentsUuid());
	}

	private void markBackpackContentsDirty() {
		BackpackStorage.get().setDirty();
	}

	@Override
	public ITrackedContentsItemResourceHandler getInventoryForInputOutput() {
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
		if (renderDataHandler == null) {
			Supplier<Runnable> getSaveHandler = () -> backpackSaveHandler;
				renderDataHandler = new RenderDataHandler(Optional.ofNullable(backpack.get(ModCoreDataComponents.RENDER_DATA)).map(RenderData::copy).orElseGet(RenderData::new), renderData -> {
					backpack.set(ModCoreDataComponents.RENDER_DATA, renderData.copy());
					getSaveHandler.get().run();
				});
		}
		renderDataValidationPending = true;
		return this;
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
		if (upgradeHandler == null) {
			if (getContentsUuid().isPresent()) {
				upgradeHandler = new UpgradeHandler(getNumberOfUpgradeSlots(), this, getBackpackContents(), this::markBackpackContentsDirty, () -> {
					if (handler != null) {
						handler.clearListeners();
						handler.setBaseSlotLimit(StackUpgradeItem.getInventorySlotLimit(this));
					}
					getInventoryHandler().clearListeners();
					handler.addListener(getSettingsHandler().getTypeCategory(ItemDisplaySettingsCategory.class)::itemChanged);
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
				upgradeHandler = Noop.INSTANCE.getUpgradeHandler();
			}
		}
		return upgradeHandler;
	}

	@Override
	public void setUpgradeCachesInvalidatedHandler(Runnable handler) {
		upgradeCachesInvalidatedHandler = handler;
	}

	private int getNumberOfUpgradeSlots() {
		Integer upgradeSlots = getBackpackStack().get(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS);

		if (upgradeSlots != null) {
			return upgradeSlots;
		}

		int itemUpgradeSlots = ((BackpackItem) getBackpackStack().getItem()).getNumberOfUpgradeSlots();
		setNumberOfUpgradeSlots(itemUpgradeSlots);
		return itemUpgradeSlots;
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
		ResourceLocation lootTable = getBackpackStack().get(ModDataComponents.LOOT_TABLE);
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
		getBackpackStack().set(ModCoreDataComponents.STORAGE_UUID, storageUuid);
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
}
