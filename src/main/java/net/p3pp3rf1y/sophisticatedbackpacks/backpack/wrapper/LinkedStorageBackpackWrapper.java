package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.energy.IEnergyStorage;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupManager;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Optional;
import java.util.UUID;
import java.util.function.IntConsumer;

public class LinkedStorageBackpackWrapper implements IBackpackWrapper {
	private BackpackWrapper physicalBackpack;
	private final IBackpackWrapper canonicalHost;
	private Runnable unsubscribe = () -> {
	};
	private Runnable inventorySlotChangeHandler = () -> {
	};
	private Runnable upgradeCachesInvalidatedHandler = () -> {
	};
	private Runnable inventoryRefreshHandler = () -> {
	};
	private Runnable inventoryInputOutputRefreshHandler = () -> {
	};
	private Runnable canonicalContentsChangedHandler = () -> {
	};
	private Runnable contentsChangeHandler = () -> {
	};
	private IntConsumer slotsChangeListener = diff -> {
	};

	public LinkedStorageBackpackWrapper(BackpackWrapper physicalBackpack, IBackpackWrapper canonicalHost) {
		this.physicalBackpack = physicalBackpack;
		this.canonicalHost = canonicalHost;
	}

	void setGroupChangeSubscription(Runnable unsubscribe) {
		this.unsubscribe = unsubscribe;
	}
	public void close() {
		unsubscribe.run();
		unsubscribe = () -> {
		};
	}
	void onCanonicalContentsChanged(LinkedStorageGroupManager.GroupChange change) {
		if (change.projectionChanged()) {
			synchronizeColumnsTaken();
			refreshPhysicalProjection();
			canonicalContentsChangedHandler.run();
		}
		inventorySlotChangeHandler.run();
		upgradeCachesInvalidatedHandler.run();
		inventoryRefreshHandler.run();
		inventoryInputOutputRefreshHandler.run();
	}

	public boolean refreshPhysicalProjection() {
		CompoundTag canonicalRenderInfo = canonicalHost.getRenderInfo().getNbt();
		if (!canonicalRenderInfo.equals(NBTHelper.getCompound(physicalBackpack.getBackpack(), BackpackRenderInfo.RENDER_INFO_TAG).orElse(null))) {
			if (!physicalBackpack.getRenderInfo().getNbt().equals(canonicalRenderInfo)) {
				physicalBackpack.getRenderInfo().deserializeFrom(canonicalRenderInfo.copy());
			}
			NBTHelper.setCompoundNBT(physicalBackpack.getBackpack(), BackpackRenderInfo.RENDER_INFO_TAG, canonicalRenderInfo.copy());
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
		canonicalContentsChangedHandler = handler;
	}
	public boolean rebindPhysicalBackpack(ItemStack backpack) {
		if (physicalBackpack.getBackpack() == backpack) {
			return false;
		}

		physicalBackpack = new BackpackWrapper(backpack);
		physicalBackpack.setContentsChangeHandler(contentsChangeHandler);
		physicalBackpack.setInventorySlotChangeHandler(inventorySlotChangeHandler);
		physicalBackpack.registerOnSlotsChangeListener(slotsChangeListener);
		return synchronizeColumnsTaken() || refreshPhysicalProjection();
	}

	@Override
	public void setContentsChangeHandler(Runnable handler) {
		contentsChangeHandler = handler;
		physicalBackpack.setContentsChangeHandler(handler);
	}
	@Override
	public void setInventorySlotChangeHandler(Runnable handler) {
		inventorySlotChangeHandler = handler;
		physicalBackpack.setInventorySlotChangeHandler(handler);
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
	public Optional<IStorageFluidHandler> getFluidHandler() {
		return canonicalHost.getFluidHandler();
	}
	@Override
	public Optional<IEnergyStorage> getEnergyStorage() {
		return canonicalHost.getEnergyStorage();
	}
	@Override
	public UpgradeHandler getUpgradeHandler() {
		return canonicalHost.getUpgradeHandler();
	}
	@Override
	public BackpackSettingsHandler getSettingsHandler() {
		return canonicalHost.getSettingsHandler();
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
	public void setOpenTabId(int id) {
		physicalBackpack.setOpenTabId(id);
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
	public void setPersistent(boolean persistent) {
		canonicalHost.setPersistent(persistent);
	}
	@Override
	public void fillWithLoot(Player player) {
		canonicalHost.fillWithLoot(player);
	}
	@Override
	public BackpackRenderInfo getRenderInfo() {
		return physicalBackpack.getRenderInfo();
	}
	@Override
	public void setColumnsTaken(int columnsTaken, boolean changed) {
		canonicalHost.setColumnsTaken(columnsTaken, false);
		physicalBackpack.setColumnsTaken(columnsTaken, changed);
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
	public void onInit(Level level) {
		canonicalHost.onInit(level);
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
	public void onContentsNbtUpdated() {
		canonicalHost.onContentsNbtUpdated();
	}
	@Override
	public ItemStack getWrappedStorageStack() {
		return physicalBackpack.getWrappedStorageStack();
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
	public ItemStack getBackpack() {
		return physicalBackpack.getBackpack();
	}
	@Override
	public ItemStack cloneBackpack() {
		ItemStack copy = physicalBackpack.cloneBackpack();
		LinkedStorageStackLifecycle.clear(copy);
		return copy;
	}
	@Override
	public void copyDataTo(IStorageWrapper other) {
		canonicalHost.copyDataTo(other);
	}
	@Override
	public void setSlotNumbers(int inventorySlots, int upgradeSlots) {
		canonicalHost.setSlotNumbers(inventorySlots, upgradeSlots);
	}
	@Override
	public void setLoot(ResourceLocation lootTableName, float percentage) {
		physicalBackpack.setLoot(lootTableName, percentage);
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
	public void setContentsUuid(UUID storageUuid) {
	}
	@Override
	public void removeContentsUuid() {
	}
	@Override
	public void removeContentsUUIDTag() {
	}
	@Override
	public void setColors(int mainColor, int accentColor) {
		physicalBackpack.setColors(mainColor, accentColor);
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
	public void setUpgradeCachesInvalidatedHandler(Runnable handler) {
		upgradeCachesInvalidatedHandler = handler;
		canonicalHost.setUpgradeCachesInvalidatedHandler(handler);
	}
	@Override
	public void registerOnSlotsChangeListener(IntConsumer listener) {
		slotsChangeListener = listener;
		physicalBackpack.registerOnSlotsChangeListener(listener);
	}
	@Override
	public void unregisterOnSlotsChangeListener() {
		slotsChangeListener = diff -> {
		};
		physicalBackpack.unregisterOnSlotsChangeListener();
	}
	@Override
	public void registerOnInventoryHandlerRefreshListener(Runnable listener) {
		inventoryRefreshHandler = listener;
		canonicalHost.registerOnInventoryHandlerRefreshListener(listener);
	}
	@Override
	public void registerOnInventoryInputOutputHandlerRefreshListener(Runnable listener) {
		inventoryInputOutputRefreshHandler = listener;
	}
	@Override
	public void unregisterOnInventoryHandlerRefreshListener() {
		canonicalHost.unregisterOnInventoryHandlerRefreshListener();
	}
}
