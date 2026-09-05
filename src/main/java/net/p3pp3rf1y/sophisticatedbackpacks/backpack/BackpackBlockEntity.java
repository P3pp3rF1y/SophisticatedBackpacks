package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.entity.ContainerOpenersCounter;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.ValueInput;
import net.minecraft.world.level.storage.ValueOutput;
import net.neoforged.neoforge.transfer.EmptyResourceHandler;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.energy.EmptyEnergyHandler;
import net.neoforged.neoforge.transfer.energy.EnergyHandler;
import net.neoforged.neoforge.transfer.fluid.FluidResource;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageJukeboxPlaybackAnchors;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.controller.ControllerBlockEntityBase;
import net.p3pp3rf1y.sophisticatedcore.controller.IControllableStorage;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageBlockEndpoint;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostDescriptor;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.util.ValueIOHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;
import org.jspecify.annotations.Nullable;

import java.util.Objects;
import java.util.Optional;
import java.util.UUID;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackBlockEntity extends BlockEntity implements IControllableStorage, ILinkedStorageBlockEndpoint {
	public static final String BACKPACK_DATA = "backpackData";
	private static final BackpackLinkedStorageEndpointAdapter STACK_ENDPOINT_ADAPTER = new BackpackLinkedStorageEndpointAdapter();
	private static final ILinkedStorageEndpointAdapter<ILinkedStorageBlockEndpoint> BLOCK_ENDPOINT_ADAPTER = new ILinkedStorageEndpointAdapter<>() {
		@Override
		public Identifier factoryId() {
			return STACK_ENDPOINT_ADAPTER.factoryId();
		}

		@Override
		public Compatibility getCompatibility(ServerLevel level, ILinkedStorageBlockEndpoint endpoint, LinkedStorageHostDescriptor hostDescriptor) {
			return STACK_ENDPOINT_ADAPTER.getCompatibility(level, ((BackpackBlockEntity) endpoint).backpackWrapper.getBackpack(), hostDescriptor);
		}

		@Override
		public LinkedStorageHostDescriptor createHostDescriptor(ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			return STACK_ENDPOINT_ADAPTER.createHostDescriptor(level, ((BackpackBlockEntity) endpoint).backpackWrapper.getBackpack());
		}

		@Override
		public ContainerContents copyCanonicalContents(ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			return STACK_ENDPOINT_ADAPTER.copyCanonicalContents(level, ((BackpackBlockEntity) endpoint).backpackWrapper.getBackpack());
		}

		@Override
		public void bindEndpoint(ServerLevel level, ILinkedStorageBlockEndpoint endpoint, LinkedStorageEndpointData endpointData) {
			STACK_ENDPOINT_ADAPTER.bindEndpoint(level, ((BackpackBlockEntity) endpoint).backpackWrapper.getBackpack(), endpointData);
		}

		@Override
		public void onEndpointLinked(ServerLevel level, ILinkedStorageBlockEndpoint endpoint) {
			((BackpackBlockEntity) endpoint).onLinkedStorageEndpointLinked();
		}
	};
	@Nullable
	private BlockPos controllerPos = null;
	private IBackpackWrapper backpackWrapper = IBackpackWrapper.Noop.INSTANCE;
	@Nullable
	private ItemStack pendingLoadedBackpack;
	private boolean updateBlockRender = true;

	private boolean chunkBeingUnloaded = false;

	@Nullable
	private ResourceHandler<ItemResource> externalItemHandler;
	@Nullable
	private ResourceHandler<FluidResource> externalFluidHandler;
	@Nullable
	private EnergyHandler externalEnergyHandler;
	private boolean triedUnpackingLoot = false;
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
		}

		@Override
		public boolean isOwnContainer(Player player) {
			return player.containerMenu instanceof BackpackContainer backpackContainer
					&& backpackContainer.getBlockPosition().map(worldPosition::equals).orElse(false);
		}
	};

	public BackpackBlockEntity(BlockPos pos, BlockState state) {
		super(BACKPACK_TILE_TYPE.get(), pos, state);
	}

	@Override
	public void setLevel(Level level) {
		super.setLevel(level);
		if (pendingLoadedBackpack != null) {
			ItemStack loadedBackpack = pendingLoadedBackpack;
			pendingLoadedBackpack = null;
			setBackpack(loadedBackpack);
		}
	}

	public void setBackpack(ItemStack backpack) {
		if (level instanceof ServerLevel serverLevel) {
			LinkedStorageJukeboxPlaybackAnchors.removeBlockAnchor(serverLevel, worldPosition, backpackWrapper.getBackpack());
		}
		if (!isCurrentWrapperOpen()) {
			closeLinkedStorageSubscription();
		}
		backpackWrapper = level == null
				? BackpackWrapper.fromStack(backpack)
				: level.isClientSide() ? new BackpackWrapper(backpack) : BackpackLinkedStorageResolver.resolveOrCreate(level, backpack);
		backpackWrapper.setContentsChangeHandler(() -> {
			setChanged();
			WorldHelper.notifyBlockUpdate(this);
		});
		backpackWrapper.setInventorySlotChangeHandler(this::setChanged);
		backpackWrapper.setUpgradeCachesInvalidatedHandler(this::invalidateHandlers);
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedStorageBackpackWrapper) {
			linkedStorageBackpackWrapper.setCanonicalContentsChangedHandler(this::refreshLinkedRenderState);
		}
		backpackWrapper.getRenderDataHandler().setRenderUpdateChangeListener(renderInfo -> {
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

	@Override
	public void loadAdditional(ValueInput in) {
		super.loadAdditional(in);
		ItemStack loadedBackpack = in.read(BACKPACK_DATA, ItemStack.CODEC).orElse(ItemStack.EMPTY);
		// This target may deserialize after onLoad, so resolve immediately once a level is attached.
		if (level == null) {
			pendingLoadedBackpack = loadedBackpack;
		} else {
			setBackpack(loadedBackpack);
		}
		loadControllerPos(in);

		if (level != null && !level.isClientSide()) {
			removeControllerPos();
			tryToAddToController();
		}

		WorldHelper.notifyBlockUpdate(this);
	}

	@Override
	public void onLoad() {
		super.onLoad();
		if (pendingLoadedBackpack != null) {
			ItemStack loadedBackpack = pendingLoadedBackpack;
			pendingLoadedBackpack = null;
			setBackpack(loadedBackpack);
		} else if (level != null && backpackWrapper != IBackpackWrapper.Noop.INSTANCE && backpackWrapper.getBackpack().getItem() instanceof BackpackItem) {
			setBackpack(backpackWrapper.getBackpack());
		}
		// Loading defers the backpack stack until onLoad, after loadAdditional already rebuilt the block with default colors.
		if (level != null && level.isClientSide()) {
			WorldHelper.notifyBlockUpdate(this);
		}
		registerWithControllerOnLoad();
	}

	private void setBackpackFrom(ValueInput in) {
		setBackpack(in.read(BACKPACK_DATA, ItemStack.CODEC).orElse(ItemStack.EMPTY));
	}

	@Override
	protected void saveAdditional(ValueOutput out) {
		super.saveAdditional(out);
		writeBackpack(out);
		saveControllerPos(out);
	}

	private void writeBackpack(ValueOutput out) {
		ItemStack backpackCopy = backpackWrapper.getBackpack().copy();
		out.store(BACKPACK_DATA, ItemStack.CODEC, backpackCopy);
	}

	@Override
	public CompoundTag getUpdateTag(HolderLookup.Provider registries) {
		CompoundTag ret = super.getUpdateTag(registries);
		return ret.merge(ValueIOHelper.collectOutputToTag(registries, out -> {
			writeBackpack(out);
			saveControllerPos(out);
			out.putBoolean("updateBlockRender", updateBlockRender);
			updateBlockRender = false;
		}));
	}

	@Nullable
	@Override
	public ClientboundBlockEntityDataPacket getUpdatePacket() {
		return ClientboundBlockEntityDataPacket.create(this);
	}

	@Override
	public void onDataPacket(Connection net, ValueInput in) {
		setBackpackFrom(in);
		if (in.getBooleanOr("updateBlockRender", false)) {
			WorldHelper.notifyBlockUpdate(this);
		}
	}

	public IBackpackWrapper getBackpackWrapper() {
		return backpackWrapper;
	}

	public void refreshClientContents(UUID contentsUuid) {
		if (level != null && level.isClientSide() && backpackWrapper.getContentsUuid().filter(contentsUuid::equals).isPresent()) {
			backpackWrapper.onContentsUpdated();
		}
	}

	@Nullable
	public LinkedStorageEndpointData getLinkedStorageEndpointData() {
		return backpackWrapper.getBackpack().get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
	}

	@Override
	public ILinkedStorageEndpointAdapter<ILinkedStorageBlockEndpoint> getLinkedStorageBlockEndpointAdapter() {
		return BLOCK_ENDPOINT_ADAPTER;
	}

	private void onLinkedStorageEndpointLinked() {
		closeMenusForThisBlock();
		setBackpack(backpackWrapper.getBackpack());
		refreshRenderState();
	}

	private void closeMenusForThisBlock() {
		if (!(level instanceof ServerLevel serverLevel)) {
			return;
		}
		for (ServerPlayer player : serverLevel.getServer().getPlayerList().getPlayers()) {
			if (player.level() == serverLevel && player.containerMenu instanceof IContextAwareContainer contextAwareContainer
					&& contextAwareContainer.getBackpackContext() instanceof BackpackContext.Block context
					&& context.getBackpackPosition(player).equals(worldPosition)) {
				player.closeContainer();
			}
		}
	}

	private void refreshLinkedRenderState() {
		// Facade projection deserializes directly into the physical carrier, bypassing its render listener.
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

	private boolean isCurrentWrapperOpen() {
		if (!(level instanceof ServerLevel serverLevel)) {
			return false;
		}
		return serverLevel.getServer().getPlayerList().getPlayers().stream()
				.anyMatch(player -> player.level() == serverLevel && player.containerMenu instanceof BackpackContainer backpackContainer
						&& backpackContainer.getBlockPosition().filter(worldPosition::equals).isPresent()
						&& backpackContainer.getStorageWrapper() == backpackWrapper);
	}

	private void invalidateHandlers() {
		invalidateCapabilities();
		externalItemHandler = null;
		externalFluidHandler = null;
		externalEnergyHandler = null;
		onInventoryInputOutputHandlerRefresh();
	}

	private boolean isBlockConnectionDisallowed(@Nullable Direction direction) {
		return direction != null && level != null
				&& Config.SERVER.noConnectionBlocks.isBlockConnectionDisallowed(level.getBlockState(getBlockPos().relative(direction)).getBlock());
	}

	@Nullable
	public ResourceHandler<ItemResource> getExternalItemHandler(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalItemHandler == null) {
			IBackpackWrapper backpackWrapper = getBackpackWrapper();
			if (!(backpackWrapper.getBackpack().getItem() instanceof BackpackItem)) {
				return EmptyResourceHandler.instance();
			}
			if (!triedUnpackingLoot && level != null && !level.isClientSide()) {
				backpackWrapper.fillWithLootAndExtraItems(level, getBlockPos());
				triedUnpackingLoot = true;
			}
			externalItemHandler = backpackWrapper.getInventoryForInputOutput();
		}
		return externalItemHandler;
	}

	@Nullable
	public ResourceHandler<FluidResource> getExternalFluidHandler(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalFluidHandler == null) {
			externalFluidHandler = getBackpackWrapper().getFluidHandler().filter(fh -> fh.size() > 0).orElse(null);
		}
		return externalFluidHandler;
	}

	@Nullable
	public EnergyHandler getExternalEnergyHandler(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalEnergyHandler == null) {
			externalEnergyHandler = getBackpackWrapper().getEnergyHandler().orElse(EmptyEnergyHandler.INSTANCE);
		}
		return externalEnergyHandler;
	}

	public void refreshRenderState() {
		BlockState state = getBlockState();
		state = state.setValue(LEFT_TANK, false);
		state = state.setValue(RIGHT_TANK, false);
		RenderDataHandler renderDataHandler = backpackWrapper.getRenderDataHandler();
		for (TankPosition pos : renderDataHandler.getTankRenderData().keySet()) {
			if (pos == TankPosition.LEFT) {
				state = state.setValue(LEFT_TANK, true);
			} else if (pos == TankPosition.RIGHT) {
				state = state.setValue(RIGHT_TANK, true);
			}
		}
		state = state.setValue(BATTERY, renderDataHandler.getBatteryRenderData().isPresent());
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
		if (level == null || level.isClientSide() || remove || player.isSpectator()) {
			return;
		}

		openersCounter.incrementOpeners(player, level, getBlockPos(), getBlockState(), player.getContainerInteractionRange());
	}

	public void stopOpen(Player player) {
		if (level == null || level.isClientSide() || remove || player.isSpectator()) {
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
		if (level.isClientSide()) {
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
		if (level != null && !level.isClientSide()) {
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

	@Override
	public void preRemoveSideEffects(BlockPos pos, BlockState state) {
		super.preRemoveSideEffects(pos, state);
		removeFromController();
	}
}
