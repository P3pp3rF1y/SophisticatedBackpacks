package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.Connection;
import net.minecraft.network.protocol.game.ClientboundBlockEntityDataPacket;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.energy.CapabilityEnergy;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.TankPosition;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackBlockEntity extends BlockEntity {
	private IBackpackWrapper backpackWrapper = NoopBackpackWrapper.INSTANCE;
	private boolean updateBlockRender = true;

	public BackpackBlockEntity(BlockPos pos, BlockState state) {
		super(BACKPACK_TILE_TYPE.get(), pos, state);
	}

	public void setBackpack(ItemStack backpack) {
		backpackWrapper = backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper.setBackpackSaveHandler(() -> {
			setChanged();
			updateBlockRender = false;
			WorldHelper.notifyBlockUpdate(this);
		});
		backpackWrapper.setInventorySlotChangeHandler(this::setChanged);
	}

	@Override
	public void load(CompoundTag nbt) {
		super.load(nbt);
		setBackpackFromNbt(nbt);
		WorldHelper.notifyBlockUpdate(this);
	}

	private void setBackpackFromNbt(CompoundTag nbt) {
		setBackpack(ItemStack.of(nbt.getCompound("backpackData")));
	}

	@Override
	protected void saveAdditional(CompoundTag tag) {
		super.saveAdditional(tag);
		writeBackpack(tag);
	}

	private void writeBackpack(CompoundTag ret) {
		ItemStack backpackCopy = backpackWrapper.getBackpack().copy();
		backpackCopy.setTag(backpackCopy.getItem().getShareTag(backpackCopy));
		ret.put("backpackData", backpackCopy.save(new CompoundTag()));
	}

	@Override
	public CompoundTag getUpdateTag() {
		CompoundTag ret = super.getUpdateTag();
		writeBackpack(ret);
		ret.putBoolean("updateBlockRender", updateBlockRender);
		updateBlockRender = true;
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

	@Override
	public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
		if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
			return LazyOptional.of(() -> getBackpackWrapper().getInventoryForInputOutput()).cast();
		} else if (cap == CapabilityFluidHandler.FLUID_HANDLER_CAPABILITY) {
			return getBackpackWrapper().getFluidHandler().<LazyOptional<T>>map(handler -> LazyOptional.of(() -> handler).cast()).orElseGet(LazyOptional::empty);
		} else if (cap == CapabilityEnergy.ENERGY) {
			return getBackpackWrapper().getEnergyStorage().<LazyOptional<T>>map(storage -> LazyOptional.of(() -> storage).cast()).orElseGet(LazyOptional::empty);
		}
		return super.getCapability(cap, side);
	}

	public void refreshRenderState() {
		BlockState state = getBlockState();
		state = state.setValue(LEFT_TANK, false);
		state = state.setValue(RIGHT_TANK, false);
		BackpackRenderInfo renderInfo = backpackWrapper.getRenderInfo();
		for (TankPosition pos : renderInfo.getTankRenderInfos().keySet()) {
			if (pos == TankPosition.LEFT) {
				state = state.setValue(LEFT_TANK, true);
			} else if (pos == TankPosition.RIGHT) {
				state = state.setValue(RIGHT_TANK, true);
			}
		}
		state = state.setValue(BATTERY, renderInfo.getBatteryRenderInfo().isPresent());
		level.setBlockAndUpdate(worldPosition, state);
		level.updateNeighborsAt(worldPosition, state.getBlock());
		WorldHelper.notifyBlockUpdate(this);
	}

	public static void serverTick(Level level, BlockPos blockPos, BackpackBlockEntity backpackBlockEntity) {
		if (level.isClientSide) {
			return;
		}
		backpackBlockEntity.backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> upgrade.tick(null, level, blockPos));
	}
}
