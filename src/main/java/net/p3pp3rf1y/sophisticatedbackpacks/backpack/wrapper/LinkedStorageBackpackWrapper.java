package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemResourceHandler;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.jspecify.annotations.Nullable;

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
	private boolean synchronizingPhysicalProjection = false;

	public LinkedStorageBackpackWrapper(BackpackWrapper physicalBackpack, IBackpackWrapper canonicalHost) {
		this.physicalBackpack = physicalBackpack;
		this.canonicalHost = canonicalHost;
		endpoint = physicalBackpack.getBackpack().get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		physicalBackpack.getRenderDataHandler().setRenderUpdateChangeListener(renderData -> synchronizeCanonicalRenderData());
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
	public ITrackedContentsItemResourceHandler getInventoryForUpgradeProcessing() {
		return canonicalHost.getInventoryForUpgradeProcessing();
	}

	@Override
	public InventoryHandler getInventoryHandler() {
		return canonicalHost.getInventoryHandler();
	}

	@Override
	public ITrackedContentsItemResourceHandler getInventoryForInputOutput() {
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
	public void onContentsUpdated() {
		canonicalHost.onContentsUpdated();
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
	public RenderDataHandler getRenderDataHandler() {
		return physicalBackpack.getRenderDataHandler();
	}

	@Override
	public void setColumnsTaken(int columnsTaken, boolean hasChanged) {
		canonicalHost.setColumnsTaken(columnsTaken, false);
		boolean columnsChanged = physicalBackpack.getColumnsTaken() != columnsTaken;
		physicalBackpack.setColumnsTaken(columnsTaken, hasChanged);
		if (columnsChanged) {
			refreshPhysicalProjection();
			onCanonicalContentsChanged.run();
		}
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
	public Optional<net.neoforged.neoforge.transfer.energy.EnergyHandler> getEnergyHandler() {
		return canonicalHost.getEnergyHandler();
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
			physicalBackpack.getRenderDataHandler().validate(this, level);
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

	public void replacePhysicalBackpackStack(ItemStack backpackStack) {
		physicalBackpack.replaceBackpackStack(backpackStack);
		physicalBackpack.getRenderDataHandler().setRenderUpdateChangeListener(renderData -> synchronizeCanonicalRenderData());
	}

	private void synchronizeCanonicalRenderData() {
		if (!synchronizingPhysicalProjection && canonicalHost instanceof BackpackLinkedStorageHostWrapper backpackHost) {
			backpackHost.synchronizeEndpointRenderData(physicalBackpack.getRenderDataHandler().getData());
		}
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
	public void setLoot(Identifier lootTableName, float lootPercentage) {
		physicalBackpack.setLoot(lootTableName, lootPercentage);
	}

	@Override
	public void setTemplate(Identifier templateName) {
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
	public Optional<net.neoforged.neoforge.transfer.ResourceHandler<net.neoforged.neoforge.transfer.fluid.FluidResource>> getItemFluidHandler() {
		return physicalBackpack.getItemFluidHandler();
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
		boolean projectionChanged = synchronizeColumnsTaken();
		projectionChanged = refreshPhysicalProjection() || projectionChanged;
		inventorySlotChangeHandler.run();
		upgradeCachesInvalidatedHandler.run();
		onInventoryHandlerRefresh.run();
		onInventoryForInputOutputHandlerRefresh.run();
		if (projectionChanged) {
			onCanonicalContentsChanged.run();
		}
	}

	public boolean refreshPhysicalProjection() {
		RenderData canonicalRenderData = canonicalHost.getRenderDataHandler().getData();
		boolean renderDataChanged = false;
		if (!physicalBackpack.getRenderDataHandler().getData().equals(canonicalRenderData)) {
			synchronizingPhysicalProjection = true;
			try {
				physicalBackpack.getRenderDataHandler().reloadFrom(canonicalRenderData);
			} finally {
				synchronizingPhysicalProjection = false;
			}
			renderDataChanged = true;
		}
		RenderData physicalRenderData = physicalBackpack.getBackpack().get(ModCoreDataComponents.RENDER_DATA);
		if (!canonicalRenderData.equals(physicalRenderData)) {
			physicalBackpack.getBackpack().set(ModCoreDataComponents.RENDER_DATA, canonicalRenderData.copy());
			renderDataChanged = true;
		}
		return renderDataChanged;
	}

	public boolean synchronizePhysicalProjection() {
		if (!refreshPhysicalProjection()) {
			return false;
		}
		onCanonicalContentsChanged.run();
		return true;
	}

	public boolean synchronizePhysicalProjection(ServerLevel level) {
		boolean columnsChanged = synchronizeColumnsTaken();
		if (endpoint == null) {
			return columnsChanged;
		}

		long renderRevision = LinkedStorageGroupsSavedData.get(level).manager().getRenderRevision(endpoint.groupId());
		ItemStack physicalStack = physicalBackpack.getBackpack();
		if (physicalStack.getOrDefault(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION, -1L) == renderRevision) {
			return columnsChanged;
		}

		boolean renderChanged = refreshPhysicalProjection();
		physicalStack.set(ModCoreDataComponents.LINKED_STORAGE_RENDER_REVISION, renderRevision);
		return columnsChanged || renderChanged;
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

}
