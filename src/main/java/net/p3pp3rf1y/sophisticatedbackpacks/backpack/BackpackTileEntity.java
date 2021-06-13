package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.block.BlockState;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.NetworkManager;
import net.minecraft.network.play.server.SUpdateTileEntityPacket;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.TankPosition;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import javax.annotation.Nullable;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.LEFT_TANK;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.RIGHT_TANK;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackTileEntity extends TileEntity implements ITickableTileEntity {
	private IBackpackWrapper backpackWrapper = NoopBackpackWrapper.INSTANCE;

	public BackpackTileEntity() {
		super(BACKPACK_TILE_TYPE.get());
	}

	public void setBackpack(ItemStack backpack) {
		backpackWrapper = backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper.setBackpackSaveHandler(this::markDirty);
	}

	@Override
	public void read(BlockState state, CompoundNBT nbt) {
		super.read(state, nbt);
		setBackpackFromNbt(nbt);
		WorldHelper.notifyBlockUpdate(this);
	}

	private void setBackpackFromNbt(CompoundNBT nbt) {
		setBackpack(ItemStack.read(nbt.getCompound("backpackData")));
	}

	@Override
	public CompoundNBT write(CompoundNBT compound) {
		CompoundNBT ret = super.write(compound);
		writeBackpack(ret);
		return ret;
	}

	private void writeBackpack(CompoundNBT ret) {
		ItemStack backpackCopy = backpackWrapper.getBackpack().copy();
		backpackCopy.setTag(backpackCopy.getItem().getShareTag(backpackCopy));
		ret.put("backpackData", backpackCopy.write(new CompoundNBT()));
	}

	@Override
	public CompoundNBT getUpdateTag() {
		CompoundNBT ret = super.getUpdateTag();
		writeBackpack(ret);
		return ret;
	}

	@Nullable
	@Override
	public SUpdateTileEntityPacket getUpdatePacket() {
		return new SUpdateTileEntityPacket(pos, 1, getUpdateTag());
	}

	@Override
	public void onDataPacket(NetworkManager net, SUpdateTileEntityPacket pkt) {
		setBackpackFromNbt(pkt.getNbtCompound());
		WorldHelper.notifyBlockUpdate(this);
	}

	public IBackpackWrapper getBackpackWrapper() {
		return backpackWrapper;
	}

	@Override
	public void tick() {
		//noinspection ConstantConditions - world is always non null at this point
		if (world.isRemote) {
			return;
		}
		backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> upgrade.tick(null, world, getPos()));
	}

	@Override
	public <T> LazyOptional<T> getCapability(Capability<T> cap, @Nullable Direction side) {
		if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
			return LazyOptional.of(() -> getBackpackWrapper().getInventoryForInputOutput()).cast();
		}
		return super.getCapability(cap, side);
	}

	public void refreshRenderState() {
		BlockState state = getBlockState();
		state = state.with(LEFT_TANK, false);
		state = state.with(RIGHT_TANK, false);
		for (TankPosition pos : backpackWrapper.getRenderInfo().getTankRenderInfos().keySet()) {
			if (pos == TankPosition.LEFT) {
				state = state.with(LEFT_TANK, true);
			} else if (pos == TankPosition.RIGHT) {
				state = state.with(RIGHT_TANK, true);
			}
		}
		world.setBlockState(pos, state);
		WorldHelper.notifyBlockUpdate(this);
	}
}
