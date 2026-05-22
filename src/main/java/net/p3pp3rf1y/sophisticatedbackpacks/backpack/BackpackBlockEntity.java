package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.HolderLookup;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.ContainerOpenersCounter;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.neoforged.neoforge.energy.IEnergyStorage;
import net.neoforged.neoforge.fluids.capability.IFluidHandler;
import net.neoforged.neoforge.fluids.capability.templates.EmptyFluidHandler;
import net.neoforged.neoforge.items.IItemHandler;
import net.neoforged.neoforge.items.wrapper.EmptyItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.EmptyEnergyStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.controller.ControllerBlockEntityBase;
import net.p3pp3rf1y.sophisticatedcore.controller.IControllableStorage;
import net.p3pp3rf1y.sophisticatedcore.inventory.CachedFailedInsertInventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackBlockEntity extends BlockEntity implements IControllableStorage {
	public static final String BACKPACK_DATA_TAG = "backpackData";
	@Nullable
	private BlockPos controllerPos = null;
	private IBackpackWrapper backpackWrapper = IBackpackWrapper.Noop.INSTANCE;
	private boolean updateBlockRender = true;

	private boolean chunkBeingUnloaded = false;

	@Nullable
	private IItemHandler externalItemHandler;
	@Nullable
	private IFluidHandler externalFluidHandler;
	@Nullable
	private IEnergyStorage externalEnergyStorage;
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
		backpackWrapper = BackpackWrapper.fromStack(backpack);
		backpackWrapper.setContentsChangeHandler(() -> {
			setChanged();
			WorldHelper.notifyBlockUpdate(this);
		});
		backpackWrapper.setInventorySlotChangeHandler(this::setChanged);
		backpackWrapper.setUpgradeCachesInvalidatedHandler(this::invalidateHandlers);
		backpackWrapper.getRenderInfo().setRenderUpdateChangeListener(renderInfo -> {
			updateBlockRender = true;
			WorldHelper.notifyBlockUpdate(this);
		});
		if (level != null && !level.isClientSide()) {
			backpackWrapper.onInit(level);
		}
	}

	@Override
	public void loadAdditional(CompoundTag tag, HolderLookup.Provider registries) {
		super.loadAdditional(tag, registries);
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
		RegistryHelper.getRegistryAccess().ifPresent(registryAccess -> setBackpack(ItemStack.parseOptional(registryAccess, nbt.getCompound(BACKPACK_DATA_TAG))));
	}

	@Override
	protected void saveAdditional(CompoundTag tag, HolderLookup.Provider registries) {
		super.saveAdditional(tag, registries);
		writeBackpack(tag, registries);
		saveControllerPos(tag);
	}

	private void writeBackpack(CompoundTag ret, HolderLookup.Provider registries) {
		ItemStack backpackCopy = backpackWrapper.getBackpack().copy();
		ret.put(BACKPACK_DATA_TAG, backpackCopy.save(registries));
	}

	@Override
	public CompoundTag getUpdateTag(HolderLookup.Provider registries) {
		CompoundTag ret = super.getUpdateTag(registries);
		writeBackpack(ret, registries);
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
	public void onDataPacket(Connection net, ClientboundBlockEntityDataPacket pkt, HolderLookup.Provider registries) {
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

	private void invalidateHandlers() {
		invalidateCapabilities();
		externalItemHandler = null;
		externalFluidHandler = null;
		externalEnergyStorage = null;
		onInventoryInputOutputHandlerRefresh();
	}

	private boolean isBlockConnectionDisallowed(@Nullable Direction direction) {
		return direction != null && level != null && Config.SERVER.noConnectionBlocks.isBlockConnectionDisallowed(level.getBlockState(getBlockPos().relative(direction)).getBlock());
	}

	@Nullable
	public IItemHandler getExternalItemHandler(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalItemHandler == null) {
			if (!(getBackpackWrapper().getBackpack().getItem() instanceof BackpackItem)) {
				return EmptyItemHandler.INSTANCE;
			}
			externalItemHandler = new CachedFailedInsertInventoryHandler<>(() -> {
				IBackpackWrapper backpackWrapper = getBackpackWrapper();
				if (!triedUnpackingLoot && level != null && !level.isClientSide()) {
					backpackWrapper.fillWithLootAndExtraItems(level, getBlockPos());
					triedUnpackingLoot = true;
				}
				return backpackWrapper.getInventoryForInputOutput();
			}, () -> level != null ? level.getGameTime() : 0);
		}
		return externalItemHandler;
	}

	@Nullable
	public IFluidHandler getExternalFluidHandler(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalFluidHandler == null) {
			externalFluidHandler = getBackpackWrapper().getFluidHandler().map(IFluidHandler.class::cast).orElse(EmptyFluidHandler.INSTANCE);
		}
		return externalFluidHandler;
	}

	@Nullable
	public IEnergyStorage getExternalEnergyStorage(@Nullable Direction direction) {
		if (isBlockConnectionDisallowed(direction)) {
			return null;
		}
		if (externalEnergyStorage == null) {
			externalEnergyStorage = getBackpackWrapper().getEnergyStorage().map(IEnergyStorage.class::cast).orElse(EmptyEnergyStorage.INSTANCE);
		}
		return externalEnergyStorage;
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
		backpackBlockEntity.backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> upgrade.tick(null, level, blockPos));
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
	}

	@Override
	public void setRemoved() {
		if (!chunkBeingUnloaded && level != null) {
			removeFromController();
		}
		super.setRemoved();
	}
}
