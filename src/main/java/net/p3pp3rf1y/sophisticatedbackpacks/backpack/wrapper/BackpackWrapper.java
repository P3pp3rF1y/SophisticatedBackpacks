package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fml.util.thread.SidedThreadGroups;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.server.ServerLifecycleHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEnergyStorageUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IFluidHandlerWrapperUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryIOHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.util.*;

import javax.annotation.Nullable;
import java.util.*;
import java.util.function.IntConsumer;

public class BackpackWrapper implements IBackpackWrapper {
	public static final int DEFAULT_CLOTH_COLOR = 13394234;
	public static final int DEFAULT_BORDER_COLOR = 6434330;
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private static final String SORT_BY_TAG = "sortBy";
	public static final String CONTENTS_UUID_TAG = "contentsUuid";
	private static final String INVENTORY_SLOTS_TAG = "inventorySlots";
	private static final String UPGRADE_SLOTS_TAG = "upgradeSlots";
	private static final String LOOT_TABLE_NAME_TAG = "lootTableName";
	private static final String LOOT_PERCENTAGE_TAG = "lootPercentage";
	private static final String COLUMNS_TAKEN_TAG = "columnsTaken";
	private static final String TEMPLATE_NAME_TAG = "templateName";

	private final ItemStack backpack;
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

	private final BackpackRenderInfo renderInfo;
	private boolean renderInfoValidationPending = true;

	private IntConsumer onSlotsChange = diff -> {
	};

	private Runnable onInventoryHandlerRefresh = () -> {
	};
	private Runnable upgradeCachesInvalidatedHandler = () -> {
	};
	private Runnable onInventoryForInputOutputHandlerRefresh = () -> {
	};

	public BackpackWrapper(ItemStack backpack) {
		this.backpack = backpack;
		renderInfo = new BackpackRenderInfo(backpack, () -> backpackSaveHandler);
	}

	@Override
	public void setContentsChangeHandler(Runnable saveHandler) {
		backpackSaveHandler = saveHandler;
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
			if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER && ServerLifecycleHooks.getCurrentServer() != null) {
				fillWithLoot(ServerLifecycleHooks.getCurrentServer().overworld(), BlockPos.ZERO);
			}
		}
		return inventoryIOHandler.getFilteredItemHandler();
	}

	@Override
	public Optional<IStorageFluidHandler> getFluidHandler() {
		if (!fluidHandlerInitialized) {
			IStorageFluidHandler wrappedHandler = getUpgradeHandler().getTypeWrappers(TankUpgradeItem.TYPE).isEmpty() ? null : new BackpackFluidHandler(this);
			List<IFluidHandlerWrapperUpgrade> fluidHandlerWrapperUpgrades = getUpgradeHandler().getWrappersThatImplement(IFluidHandlerWrapperUpgrade.class);

			for (IFluidHandlerWrapperUpgrade fluidHandlerWrapperUpgrade : fluidHandlerWrapperUpgrades) {
				wrappedHandler = fluidHandlerWrapperUpgrade.wrapHandler(wrappedHandler, backpack);
			}

			fluidHandler = wrappedHandler;
			fluidHandlerInitialized = true;
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
						//noinspection ConstantConditions - by this time the upgrade has registryName so it can't be null
						return super.isItemValid(slot, stack) && (stack.isEmpty() || SophisticatedBackpacks.MOD_ID.equals(ForgeRegistries.ITEMS.getKey(stack.getItem()).getNamespace()) || stack.is(ModItems.BACKPACK_UPGRADE_TAG));
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
		if (upgradeHandler == Noop.INSTANCE.getUpgradeHandler()) {
			upgradeHandler = null;
		}
		if (settingsHandler == Noop.INSTANCE.getSettingsHandler()) {
			settingsHandler = null;
		}
	}

	private void migrateBackpackContents(UUID newUuid) {
		migrateNbtTag(newUuid, InventoryHandler.INVENTORY_TAG);
		migrateNbtTag(newUuid, UpgradeHandler.UPGRADE_INVENTORY_TAG);
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
	public int getMainColor() {
		return BackpackItem.getMainColor(backpack);
	}

	@Override
	public int getAccentColor() {
		return BackpackItem.getAccentColor(backpack);
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
	public void setColors(int mainColor, int accentColor) {
		BackpackItem.setColors(backpack, mainColor, accentColor);
		backpackSaveHandler.run();
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		backpack.addTagElement(SORT_BY_TAG, StringTag.valueOf(sortBy.getSerializedName()));
		backpackSaveHandler.run();
	}

	@Override
	public SortBy getSortBy() {
		return NBTHelper.getEnumConstant(backpack, SORT_BY_TAG, SortBy::fromName).orElse(SortBy.NAME);
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
		return backpack;
	}

	@Override
	public ItemStack cloneBackpack() {
		ItemStack clonedBackpack = cloneBackpack(this);
		clonedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(this::cloneSubbackpacks);
		return clonedBackpack;
	}

	private void cloneSubbackpacks(IStorageWrapper wrapperCloned) {
		InventoryHandler inventoryHandler = wrapperCloned.getInventoryHandler();
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
	public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
		backpack.addTagElement(LOOT_TABLE_NAME_TAG, StringTag.valueOf(lootTableName.toString()));
		backpack.addTagElement(LOOT_PERCENTAGE_TAG, FloatTag.valueOf(lootPercentage));
		backpackSaveHandler.run();
	}

	@Override
	public void setTemplate(ResourceLocation templateName) {
		NBTHelper.putString(backpack.getOrCreateTag(), TEMPLATE_NAME_TAG, templateName.toString());
	}

	@Override
	public void fillWithLoot(Player player) {
		Level level = player.level();
		if (level.isClientSide) {
			return;
		}
		fillFromTemplate();
		fillWithLoot(level, player.blockPosition(), player);
	}

	private void fillWithLoot(Level level, BlockPos pos) {
		NBTHelper.getString(backpack, LOOT_TABLE_NAME_TAG).ifPresent(ltName -> fillWithLootFromTable(level, pos, ltName, null));
	}

	public void fillWithLoot(Level level, BlockPos pos, @Nullable Player player) {
		NBTHelper.getString(backpack, LOOT_TABLE_NAME_TAG).ifPresent(ltName -> fillWithLootFromTable(level, pos, ltName, player));
	}

	@Override
	public void fillFromTemplate() {
		ResourceLocation templateName = NBTHelper.getString(backpack, TEMPLATE_NAME_TAG).map(ResourceLocation::new).orElse(null);
		if (templateName == null) {
			return;
		}

		Optional<CompoundTag> templateData = BackpackTemplates.getBackpackTemplate(templateName);
		if (templateData.isEmpty()) {
			return;
		}

		CompoundTag backpackContent = templateData.get().getCompound("backpackContents").copy();
		BackpackStorage.get().setBackpackContents(getOrCreateContentsUuid(), backpackContent);
		NBTHelper.removeTag(backpack, TEMPLATE_NAME_TAG);
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		NBTHelper.setUniqueId(backpack, CONTENTS_UUID_TAG, storageUuid);
	}

	@Override
	public void removeContentsUuid() {
		getContentsUuid().ifPresent(BackpackStorage.get()::removeBackpackContents);
		removeContentsUUIDTag();
	}

	@Override
	public void removeContentsUUIDTag() {
		NBTHelper.removeTag(backpack, CONTENTS_UUID_TAG);
	}

	@Override
	public BackpackRenderInfo getRenderInfo() {
		return renderInfo;
	}

	@Override
	public void setColumnsTaken(int columnsTaken, boolean hasChanged) {
		int originalColumnsTaken = getColumnsTaken();
		NBTHelper.setInteger(backpack, COLUMNS_TAKEN_TAG, columnsTaken);
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
		return NBTHelper.getInt(backpack, COLUMNS_TAKEN_TAG).orElse(0);
	}

	private void fillWithLootFromTable(Level level, BlockPos pos, String lootName, @Nullable Player player) {
		MinecraftServer server = level.getServer();
		if (server == null || !(level instanceof ServerLevel serverLevel)) {
			return;
		}

		ResourceLocation lootTableName = new ResourceLocation(lootName);
		float lootPercentage = NBTHelper.getFloat(backpack, LOOT_PERCENTAGE_TAG).orElse(0f);

		backpack.removeTagKey(LOOT_TABLE_NAME_TAG);
		backpack.removeTagKey(LOOT_PERCENTAGE_TAG);

		List<ItemStack> loot = new ArrayList<>();
		while (lootPercentage > 0) {
			List<ItemStack> generatedLoot = LootHelper.getLoot(lootTableName, server, serverLevel, pos, player);
			generatedLoot.removeIf(stack -> stack.getItem() instanceof BackpackItem);
			loot.addAll(RandHelper.getNRandomElements(generatedLoot, (int) (generatedLoot.size() * (lootPercentage > 1 ? 1 : lootPercentage))));
			lootPercentage--;
		}
		LootHelper.fillWithLoot(serverLevel.random, loot, getInventoryHandler());
	}

	private void setNumberOfUpgradeSlots(int numberOfUpgradeSlots) {
		NBTHelper.setInteger(backpack, UPGRADE_SLOTS_TAG, numberOfUpgradeSlots);
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
}
