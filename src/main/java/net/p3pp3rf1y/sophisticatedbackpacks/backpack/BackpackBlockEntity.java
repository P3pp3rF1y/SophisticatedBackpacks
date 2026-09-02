package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.ContainerOpenersCounter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.ForgeCapabilities;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.templates.EmptyFluidHandler;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.wrapper.EmptyHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.EmptyEnergyStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageJukeboxPlaybackAnchors;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.controller.ControllerBlockEntityBase;
import net.p3pp3rf1y.sophisticatedcore.controller.IControllableStorage;
import net.p3pp3rf1y.sophisticatedcore.inventory.CachedFailedInsertInventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageBlockEndpoint;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.Objects;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.OPEN;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

@SuppressWarnings("PMD.UnnecessaryImport")
public class BackpackBlockEntity extends BlockEntity implements IControllableStorage, ILinkedStorageBlockEndpoint {
	public static final String BACKPACK_DATA_TAG = "backpackData";
	@Nullable
	private BlockPos controllerPos = null;
	private IBackpackWrapper backpackWrapper = IBackpackWrapper.Noop.INSTANCE;
	private boolean updateBlockRender = true;

	private boolean chunkBeingUnloaded = false;
	private static final BackpackLinkedStorageEndpointAdapter ITEM_ENDPOINT_ADAPTER = new BackpackLinkedStorageEndpointAdapter();
	private static final ILinkedStorageEndpointAdapter<ILinkedStorageBlockEndpoint> BLOCK_ENDPOINT_ADAPTER = new ILinkedStorageEndpointAdapter<>() {
		@Override
		public net.minecraft.resources.ResourceLocation factoryId() {
			return ITEM_ENDPOINT_ADAPTER.factoryId();
		}

		@Override
		public Compatibility getCompatibility(net.minecraft.server.level.ServerLevel level, ILinkedStorageBlockEndpoint endpoint,
				LinkedStorageHostDescriptor descriptor) {
			return ITEM_ENDPOINT_ADAPTER.getCompatibility(level, ((BackpackBlockEntity) endpoint).getBackpackWrapper().getBackpack(), descriptor);
		}

		@Override
		public LinkedStorageHostDescriptor createHostDescriptor(net.minecraft.server.level.ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			return ITEM_ENDPOINT_ADAPTER.createHostDescriptor(level, ((BackpackBlockEntity) endpoint).getBackpackWrapper().getBackpack());
		}

		@Override
		public CompoundTag copyCanonicalContents(net.minecraft.server.level.ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			return ITEM_ENDPOINT_ADAPTER.copyCanonicalContents(level, ((BackpackBlockEntity) endpoint).getBackpackWrapper().getBackpack());
		}

		@Override
		public void bindEndpoint(net.minecraft.server.level.ServerLevel level, ILinkedStorageBlockEndpoint endpoint, LinkedStorageEndpointData endpointData) {
			BackpackBlockEntity blockEntity = (BackpackBlockEntity) endpoint;
			ItemStack stack = blockEntity.getBackpackWrapper().getBackpack();
			ITEM_ENDPOINT_ADAPTER.bindEndpoint(level, stack, endpointData);
		}

		@Override
		public void onEndpointLinked(net.minecraft.server.level.ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			BackpackBlockEntity blockEntity = (BackpackBlockEntity) endpoint;
			blockEntity.closeMenusForThisBlock();
			blockEntity.setBackpack(blockEntity.getBackpackWrapper().getBackpack());
			blockEntity.refreshRenderState();
		}
	};

	@Nullable
	private LazyOptional<IItemHandler> itemHandlerCap;
	@Nullable
	private LazyOptional<IFluidHandler> fluidHandlerCap;
	@Nullable
	private LazyOptional<IEnergyStorage> energyStorageCap;
	private final ContainerOpenersCounter openersCounter = new ContainerOpenersCounter() {
		@Override
		protected void onOpen(Level level, BlockPos pos, BlockState state) {
			setOpenBlockState(state, true);
		}

		@Override
		protected void onClose(Level level, BlockPos pos, BlockState state) {
			setOpenBlockState(state, false);
		}

		@Override
		protected void openerCountChanged(Level level, BlockPos pos, BlockState state, int count, int openCount) {
			// noop
		}

		@Override
		protected boolean isOwnContainer(Player player) {
			return player.containerMenu instanceof BackpackContainer backpackContainer
					&& backpackContainer.getBlockPosition().map(worldPosition::equals).orElse(false);
		}
	};

	public BackpackBlockEntity(BlockPos pos, BlockState state) {
		super(BACKPACK_TILE_TYPE.get(), pos, state);
	}

	public void setBackpack(ItemStack backpack) {
		if (level instanceof ServerLevel serverLevel) {
			LinkedStorageJukeboxPlaybackAnchors.removeBlockAnchor(serverLevel, worldPosition, backpackWrapper.getBackpack());
		}
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper
				&& BackpackLinkedStorageResolver.hasSameEndpoint(backpackWrapper.getBackpack(), backpack)) {
			boolean projectionChanged = linkedStorageBackpackWrapper.rebindPhysicalBackpack(backpack);
			if (level instanceof ServerLevel serverLevel) {
				backpackWrapper.onInit(level);
				LinkedStorageJukeboxPlaybackAnchors.refreshBlockAnchor(serverLevel, worldPosition, backpackWrapper.getBackpack());
			}
			if (projectionChanged) {
				refreshLinkedRenderState();
			}
			return;
		}
		closeLinkedStorageSubscription();
		backpackWrapper = level == null || level.isClientSide ? new BackpackWrapper(backpack) : BackpackLinkedStorageResolver.resolveOrCreate(level, backpack);
		backpackWrapper.setContentsChangeHandler(() -> {
			setChanged();
			WorldHelper.notifyBlockUpdate(this);
		});
		backpackWrapper.setInventorySlotChangeHandler(this::setChanged);
		backpackWrapper.setUpgradeCachesInvalidatedHandler(this::invalidateBackpackCaps);
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.setCanonicalContentsChangedHandler(this::refreshLinkedRenderState);
		}
		backpackWrapper.getRenderInfo().setRenderUpdateChangeListener(renderInfo -> {
			updateBlockRender = true;
			WorldHelper.notifyBlockUpdate(this);
		});
		if (level != null && !level.isClientSide()) {
			backpackWrapper.onInit(level);
			LinkedStorageJukeboxPlaybackAnchors.refreshBlockAnchor((ServerLevel) level, worldPosition, backpackWrapper.getBackpack());
			if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
				linkedStorageBackpackWrapper.refreshPhysicalProjection();
				refreshLinkedRenderState();
			}
		}
	}

	private void closeMenusForThisBlock() {
		if (!(level instanceof ServerLevel serverLevel)) {
			return;
		}
		for (ServerPlayer player : serverLevel.getServer().getPlayerList().getPlayers()) {
			if (player.serverLevel() == serverLevel && player.containerMenu instanceof IContextAwareContainer container
					&& container.getBackpackContext().getBackpackPosition(player).equals(worldPosition)) {
				player.closeContainer();
			}
		}
	}

	private void refreshLinkedRenderState() {
		updateBlockRender = true;
		setChanged();
		if (level != null && !level.isClientSide()) {
			refreshRenderState();
		}
		WorldHelper.notifyBlockUpdate(this);
	}

	private void closeLinkedStorageSubscription() {
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.close();
		}
	}

	@Override
	public void load(CompoundTag tag) {
		super.load(tag);
		setBackpackFromNbt(tag);
		loadControllerPos(tag);

		if (level != null && !level.isClientSide()) {
			removeControllerPos();
			tryToAddToController();
		}

		WorldHelper.notifyBlockUpdate(this);
	}

	@Override
	public void onLoad() {
		super.onLoad();
		registerWithControllerOnLoad();
	}

	private void setBackpackFromNbt(CompoundTag nbt) {
		setBackpack(ItemStack.of(nbt.getCompound(BACKPACK_DATA_TAG)));
	}

	@Override
	protected void saveAdditional(CompoundTag tag) {
		super.saveAdditional(tag);
		writeBackpack(tag);
		saveControllerPos(tag);
	}

	private void writeBackpack(CompoundTag ret) {
		ItemStack backpackCopy = backpackWrapper.getBackpack().copy();
		backpackCopy.setTag(backpackCopy.getItem().getShareTag(backpackCopy));
		ret.put(BACKPACK_DATA_TAG, backpackCopy.save(new CompoundTag()));
	}

	@Override
	public CompoundTag getUpdateTag() {
		CompoundTag ret = super.getUpdateTag();
		writeBackpack(ret);
		saveControllerPos(ret);
		ret.putBoolean("updateBlockRender", updateBlockRender);
		updateBlockRender = false;
		return ret;
	}

	@Nullable
	@Override
	public ClientboundBlockEntityDataPacket getUpdatePacket() {
		return ClientboundBlockEntityDataPacket.create(this);
	}

	@Override
	public void onDataPacket(Connection net, ClientboundBlockEntityDataPacket pkt) {
		CompoundTag tag = pkt.getTag();
		if (tag == null) {
			return;
		}

		setBackpackFromNbt(tag);
		if (tag.getBoolean("updateBlockRender")) {
			WorldHelper.notifyBlockUpdate(this);
		}
	}

	public IBackpackWrapper getBackpackWrapper() {
		return backpackWrapper;
	}

	@Nullable
	@Override
	public LinkedStorageEndpointData getLinkedStorageEndpointData() {
		return LinkedStorageStackData.getEndpoint(backpackWrapper.getBackpack());
	}

	@Override
	public ILinkedStorageEndpointAdapter<ILinkedStorageBlockEndpoint> getLinkedStorageBlockEndpointAdapter() {
		return BLOCK_ENDPOINT_ADAPTER;
	}

	@Nonnull
	@Override
	public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
		if (side != null && level != null
				&& Config.SERVER.noConnectionBlocks.isBlockConnectionDisallowed(level.getBlockState(getBlockPos().relative(side)).getBlock())) {
			return super.getCapability(cap, side);
		}

		if (cap == ForgeCapabilities.ITEM_HANDLER) {
			if (!(getBackpackWrapper().getBackpack().getItem() instanceof BackpackItem)) {
				return LazyOptional.of(() -> EmptyHandler.INSTANCE).cast();
			}
			if (itemHandlerCap == null) {
				itemHandlerCap = LazyOptional.of(() -> new CachedFailedInsertInventoryHandler<>(() -> getBackpackWrapper().getInventoryForInputOutput(),
						() -> level != null ? level.getGameTime() : 0));
			}
			return itemHandlerCap.cast();
		} else if (cap == ForgeCapabilities.FLUID_HANDLER) {
			if (fluidHandlerCap == null) {
				fluidHandlerCap = LazyOptional
						.of(() -> getBackpackWrapper().getFluidHandler().map(IFluidHandler.class::cast).orElse(EmptyFluidHandler.INSTANCE));
			}
			return fluidHandlerCap.cast();
		} else if (cap == ForgeCapabilities.ENERGY) {
			if (energyStorageCap == null) {
				energyStorageCap = LazyOptional
						.of(() -> getBackpackWrapper().getEnergyStorage().map(IEnergyStorage.class::cast).orElse(EmptyEnergyStorage.INSTANCE));
			}
			return energyStorageCap.cast();
		}
		return super.getCapability(cap, side);
	}

	@Override
	public void invalidateCaps() {
		super.invalidateCaps();
		invalidateBackpackCaps();
	}

	private void invalidateBackpackCaps() {
		if (itemHandlerCap != null) {
			LazyOptional<IItemHandler> tempItemHandlerCap = itemHandlerCap;
			itemHandlerCap = null;
			tempItemHandlerCap.invalidate();
		}
		onInventoryInputOutputHandlerRefresh();
		if (fluidHandlerCap != null) {
			LazyOptional<IFluidHandler> tempFluidHandlerCap = fluidHandlerCap;
			fluidHandlerCap = null;
			tempFluidHandlerCap.invalidate();
		}
		if (energyStorageCap != null) {
			LazyOptional<IEnergyStorage> tempEnergyStorageCap = energyStorageCap;
			energyStorageCap = null;
			tempEnergyStorageCap.invalidate();
		}
	}

	public void refreshRenderState() {
		BlockState state = getBlockState();
		state = state.setValue(LEFT_TANK, false);
		state = state.setValue(RIGHT_TANK, false);
		RenderInfo renderInfo = backpackWrapper.getRenderInfo();
		for (TankPosition pos : renderInfo.getTankRenderInfos().keySet()) {
			if (pos == TankPosition.LEFT) {
				state = state.setValue(LEFT_TANK, true);
			} else if (pos == TankPosition.RIGHT) {
				state = state.setValue(RIGHT_TANK, true);
			}
		}
		state = state.setValue(BATTERY, renderInfo.getBatteryRenderInfo().isPresent());
		Level l = Objects.requireNonNull(level);
		l.setBlockAndUpdate(worldPosition, state);
		l.updateNeighborsAt(worldPosition, state.getBlock());
		WorldHelper.notifyBlockUpdate(this);
	}

	private void setOpenBlockState(BlockState state, boolean open) {
		if (level == null || !state.hasProperty(OPEN) || state.getValue(OPEN) == open) {
			return;
		}

		level.setBlock(worldPosition, state.setValue(OPEN, open), 3);
	}

	public void startOpen(Player player) {
		if (level == null || level.isClientSide || remove || player.isSpectator()) {
			return;
		}

		openersCounter.incrementOpeners(player, level, getBlockPos(), getBlockState());
	}

	public void stopOpen(Player player) {
		if (level == null || level.isClientSide || remove || player.isSpectator()) {
			return;
		}

		openersCounter.decrementOpeners(player, level, getBlockPos(), getBlockState());
	}

	public void recheckOpen() {
		if (!remove && level != null) {
			openersCounter.recheckOpeners(level, getBlockPos(), getBlockState());
		}
	}

	public static void serverTick(Level level, BlockPos blockPos, BackpackBlockEntity backpackBlockEntity) {
		if (level.isClientSide) {
			return;
		}
		if (level instanceof ServerLevel serverLevel && LinkedStorageStackLifecycle
				.classifyEndpoint(backpackBlockEntity.backpackWrapper.getBackpack()) == LinkedStorageEndpointStackState.ENDPOINT) {
			BackpackLinkedStorageResolver.resolvePrimaryCanonicalHost(serverLevel, backpackBlockEntity.backpackWrapper.getBackpack())
					.ifPresent(backpackWrapper -> backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
							.forEach(upgrade -> upgrade.tick(null, level, blockPos)));
			return;
		}
		backpackBlockEntity.backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class)
				.forEach(upgrade -> upgrade.tick(null, level, blockPos));
	}

	@Override
	public IStorageWrapper getStorageWrapper() {
		return backpackWrapper;
	}

	@Override
	public void setControllerPos(BlockPos controllerPos) {
		this.controllerPos = controllerPos;
		setChanged();
	}

	@Override
	public Optional<BlockPos> getControllerPos() {
		return Optional.ofNullable(controllerPos);
	}

	@Override
	public void removeControllerPos() {
		controllerPos = null;
	}

	@Override
	public BlockPos getStorageBlockPos() {
		return getBlockPos();
	}

	@Override
	public Level getStorageBlockLevel() {
		return Objects.requireNonNull(getLevel());
	}

	@Override
	public boolean canConnectStorages() {
		return false;
	}

	@Override
	public void unregisterController() {
		IControllableStorage.super.unregisterController();
		backpackWrapper.unregisterOnSlotsChangeListener();
		backpackWrapper.unregisterOnInventoryHandlerRefreshListener();
	}

	@Override
	public void registerController(ControllerBlockEntityBase controllerBlockEntity) {
		IControllableStorage.super.registerController(controllerBlockEntity);
		if (level != null && !level.isClientSide) {
			backpackWrapper.registerOnSlotsChangeListener(this::changeSlots);
			backpackWrapper.registerOnInventoryHandlerRefreshListener(this::registerInventoryStackListeners);
		}
	}

	@Override
	public void onChunkUnloaded() {
		super.onChunkUnloaded();
		chunkBeingUnloaded = true;
		if (level instanceof ServerLevel serverLevel) {
			LinkedStorageJukeboxPlaybackAnchors.removeBlockAnchor(serverLevel, worldPosition, backpackWrapper.getBackpack());
		}
		closeLinkedStorageSubscription();
	}

	@Override
	public void setRemoved() {
		if (!chunkBeingUnloaded && level != null) {
			removeFromController();
		}
		if (level instanceof ServerLevel serverLevel) {
			LinkedStorageJukeboxPlaybackAnchors.removeBlockAnchor(serverLevel, worldPosition, backpackWrapper.getBackpack());
		}
		closeLinkedStorageSubscription();
		super.setRemoved();
	}
}
