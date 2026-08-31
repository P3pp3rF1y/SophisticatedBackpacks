package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.energy.IEnergyStorage;
import net.neoforged.neoforge.fluids.FluidStack;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.IFluidHandlerItem;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import javax.annotation.Nullable;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.IntConsumer;

public class LinkedStorageBackpackWrapper implements IBackpackWrapper {
	private final BackpackWrapper physicalBackpack;
	private final IBackpackWrapper canonicalHost;
	@Nullable
	private final LinkedStorageEndpointData endpoint;
	private Runnable inventorySlotChangeHandler = () -> {
	};
	private Runnable upgradeCachesInvalidatedHandler = () -> {
	};
	private Runnable onInventoryHandlerRefresh = () -> {
	};
	private Runnable onInventoryForInputOutputHandlerRefresh = () -> {
	};
	private Runnable onCanonicalContentsChanged = () -> {
	};
	private Runnable groupChangeSubscription = () -> {
	};

	public LinkedStorageBackpackWrapper(BackpackWrapper physicalBackpack, IBackpackWrapper canonicalHost) {
		this.physicalBackpack = physicalBackpack;
		this.canonicalHost = canonicalHost;
		endpoint = physicalBackpack.getBackpack().get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
	}

	@Override
	public void setContentsChangeHandler(Runnable contentsChangeHandler) {
		physicalBackpack.setContentsChangeHandler(contentsChangeHandler);
	}

	@Override
	public void setInventorySlotChangeHandler(Runnable slotChangeHandler) {
		inventorySlotChangeHandler = slotChangeHandler;
	}

	@Override
	public ITrackedContentsItemHandler getInventoryForUpgradeProcessing() {
		return canonicalHost.getInventoryForUpgradeProcessing();
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		return canonicalHost.getInventoryHandler();
	}

	@Override
	public ITrackedContentsItemHandler getInventoryForInputOutput() {
		return canonicalHost.getInventoryForInputOutput();
	}

	@Override
	public void setUpgradeCachesInvalidatedHandler(Runnable handler) {
		upgradeCachesInvalidatedHandler = handler;
	}

	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		return canonicalHost.getSettingsHandler();
	}

	@Override
	public UpgradeHandler getUpgradeHandler() {
		return canonicalHost.getUpgradeHandler();
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return canonicalHost.getContentsUuid();
	}

	@Override
	public Optional<Integer> getOpenTabId() {
		return physicalBackpack.getOpenTabId();
	}

	@Override
	public void setOpenTabId(int openTabId) {
		physicalBackpack.setOpenTabId(openTabId);
	}

	@Override
	public void removeOpenTabId() {
		physicalBackpack.removeOpenTabId();
	}

	@Override
	public void setSortBy(SortBy sortBy) {
		canonicalHost.setSortBy(sortBy);
	}

	@Override
	public SortBy getSortBy() {
		return canonicalHost.getSortBy();
	}

	@Override
	public void sort() {
		canonicalHost.sort();
	}

	@Override
	public void onContentsNbtUpdated() {
		canonicalHost.onContentsNbtUpdated();
	}

	@Override
	public void refreshInventoryForUpgradeProcessing() {
		canonicalHost.refreshInventoryForUpgradeProcessing();
	}

	@Override
	public void refreshInventoryForInputOutput() {
		canonicalHost.refreshInventoryForInputOutput();
	}

	@Override
	public void setPersistent(boolean persistent) {
		canonicalHost.setPersistent(persistent);
	}

	@Override
	public void fillWithLoot(Player playerEntity) {
		canonicalHost.fillWithLoot(playerEntity);
	}

	@Override
	public RenderInfo getRenderInfo() {
		return physicalBackpack.getRenderInfo();
	}

	@Override
	public void setColumnsTaken(int columnsTaken, boolean hasChanged) {
		canonicalHost.setColumnsTaken(columnsTaken, false);
		physicalBackpack.setColumnsTaken(columnsTaken, hasChanged);
	}

	@Override
	public int getColumnsTaken() {
		return canonicalHost.getColumnsTaken();
	}

	@Override
	public int getNumberOfSlotRows() {
		return canonicalHost.getNumberOfSlotRows();
	}

	@Override
	public Optional<IStorageFluidHandler> getFluidHandler() {
		return canonicalHost.getFluidHandler();
	}

	@Override
	public Optional<IEnergyStorage> getEnergyStorage() {
		return canonicalHost.getEnergyStorage();
	}

	@Override
	public ItemStack getWrappedStorageStack() {
		return physicalBackpack.getWrappedStorageStack();
	}

	@Override
	public int getBaseStackSizeMultiplier() {
		return canonicalHost.getBaseStackSizeMultiplier();
	}

	@Override
	public void onInit(Level level) {
		canonicalHost.onInit(level);
		if (!level.isClientSide()) {
			physicalBackpack.getRenderInfo().validate(this, level);
		}
	}

	@Override
	public String getStorageType() {
		return physicalBackpack.getStorageType();
	}

	@Override
	public Component getDisplayName() {
		return canonicalHost.getDisplayName();
	}

	@Override
	public boolean isUpgradeRunnable(ItemStack upgrade) {
		return canonicalHost.isUpgradeRunnable(upgrade);
	}

	@Override
	public void registerOnInventoryInputOutputHandlerRefreshListener(Runnable onInventoryForInputOutputHandlerRefresh) {
		this.onInventoryForInputOutputHandlerRefresh = onInventoryForInputOutputHandlerRefresh;
	}

	@Override
	public IBackpackWrapper setBackpackStack(ItemStack backpackStack) {
		physicalBackpack.setBackpackStack(backpackStack);
		return this;
	}

	public boolean hasEndpoint(@Nullable LinkedStorageEndpointData endpoint) {
		return this.endpoint != null && this.endpoint.equals(endpoint);
	}

	@Override
	public ItemStack getBackpack() {
		return physicalBackpack.getBackpack();
	}

	@Override
	public ItemStack cloneBackpack() {
		ItemStack clone = physicalBackpack.cloneBackpack();
		LinkedStorageStackLifecycle.clear(clone);
		return clone;
	}

	@Override
	public void copyDataTo(IStorageWrapper otherStorageWrapper) {
		canonicalHost.copyDataTo(otherStorageWrapper);
	}

	@Override
	public void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots) {
		canonicalHost.setSlotNumbers(numberOfInventorySlots, numberOfUpgradeSlots);
	}

	@Override
	public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
		physicalBackpack.setLoot(lootTableName, lootPercentage);
	}

	@Override
	public void setTemplate(ResourceLocation templateName) {
		physicalBackpack.setTemplate(templateName);
	}

	@Override
	public void fillFromTemplate() {
		canonicalHost.fillFromTemplate();
	}

	@Override
	public void fillWithLootAndExtraItems(Level level, BlockPos pos) {
		canonicalHost.fillWithLootAndExtraItems(level, pos);
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		// The canonical host owns the linked-storage group identity.
	}

	@Override
	public void removeContentsUuid() {
		// Removing this facade must never delete the canonical group's contents.
	}

	@Override
	public void removeContentsUUIDTag() {
		// The physical carrier has no local contents identity while linked.
	}

	@Override
	public void registerOnSlotsChangeListener(IntConsumer onSlotsChange) {
		physicalBackpack.registerOnSlotsChangeListener(onSlotsChange);
	}

	@Override
	public void unregisterOnSlotsChangeListener() {
		physicalBackpack.unregisterOnSlotsChangeListener();
	}

	@Override
	public void registerOnInventoryHandlerRefreshListener(Runnable onInventoryHandlerRefresh) {
		this.onInventoryHandlerRefresh = onInventoryHandlerRefresh;
	}

	@Override
	public void unregisterOnInventoryHandlerRefreshListener() {
		onInventoryHandlerRefresh = () -> {
		};
	}

	@Override
	public Optional<IFluidHandlerItem> getItemFluidHandler() {
		return canonicalHost.getFluidHandler().map(fluidHandler -> new PhysicalFluidHandlerItem(physicalBackpack.getBackpack(), fluidHandler));
	}

	@Override
	public int getMainColor() {
		return physicalBackpack.getMainColor();
	}

	@Override
	public int getAccentColor() {
		return physicalBackpack.getAccentColor();
	}

	@Override
	public void setColors(int mainColor, int accentColor) {
		physicalBackpack.setColors(mainColor, accentColor);
	}

	void setGroupChangeSubscription(Runnable groupChangeSubscription) {
		this.groupChangeSubscription = groupChangeSubscription;
	}

	public void close() {
		groupChangeSubscription.run();
		groupChangeSubscription = () -> {
		};
	}

	void onCanonicalContentsChanged() {
		boolean physicalProjectionChanged = synchronizeColumnsTaken() | refreshPhysicalProjection();
		inventorySlotChangeHandler.run();
		upgradeCachesInvalidatedHandler.run();
		onInventoryHandlerRefresh.run();
		onInventoryForInputOutputHandlerRefresh.run();
		if (physicalProjectionChanged) {
			onCanonicalContentsChanged.run();
		}
	}

	public boolean refreshPhysicalProjection() {
		CompoundTag canonicalRenderInfo = canonicalHost.getRenderInfo().getNbt();
		CustomData physicalRenderInfo = physicalBackpack.getBackpack().get(ModCoreDataComponents.RENDER_INFO_TAG);
		if (physicalRenderInfo == null || !Objects.equals(physicalRenderInfo.copyTag(), canonicalRenderInfo)) {
			if (!physicalBackpack.getRenderInfo().getNbt().equals(canonicalRenderInfo)) {
				physicalBackpack.getRenderInfo().deserializeFrom(canonicalRenderInfo.copy());
			}
			physicalBackpack.getBackpack().set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(canonicalRenderInfo.copy()));
			return true;
		}
		return false;
	}

	private boolean synchronizeColumnsTaken() {
		int canonicalColumnsTaken = canonicalHost.getColumnsTaken();
		if (physicalBackpack.getColumnsTaken() != canonicalColumnsTaken) {
			physicalBackpack.setColumnsTaken(canonicalColumnsTaken, false);
			return true;
		}
		return false;
	}

	public void setCanonicalContentsChangedHandler(Runnable handler) {
		onCanonicalContentsChanged = handler;
	}

	private record PhysicalFluidHandlerItem(ItemStack container, IFluidHandler delegate) implements IFluidHandlerItem {
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
