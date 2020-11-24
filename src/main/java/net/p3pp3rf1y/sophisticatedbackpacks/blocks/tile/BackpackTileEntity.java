package net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile;

import net.minecraft.block.BlockState;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.Direction;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.util.LazyOptional;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NoopBackpackWrapper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackTileEntity extends TileEntity implements ITickableTileEntity {
	private IBackpackWrapper backpackWrapper;

	public BackpackTileEntity() {
		super(BACKPACK_TILE_TYPE.get());
	}

	public void setBackpack(ItemStack backpack) {
		backpackWrapper = backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).orElse(NoopBackpackWrapper.INSTANCE);
		backpackWrapper.linkToTileEntity(this);
	}

	@Override
	public void read(BlockState state, CompoundNBT nbt) {
		super.read(state, nbt);
		setBackpack(ItemStack.read(nbt.getCompound("backpackData")));
	}

	@Override
	public CompoundNBT write(CompoundNBT compound) {
		CompoundNBT ret = super.write(compound);
		writeBackpack(ret);
		return ret;
	}

	private void writeBackpack(CompoundNBT ret) {
		ret.put("backpackData", backpackWrapper.getBackpack().write(new CompoundNBT()));
	}

	@Override
	public CompoundNBT getUpdateTag() {
		CompoundNBT ret = super.getUpdateTag();
		writeBackpack(ret);
		return ret;
	}

	public Optional<IBackpackWrapper> getBackpackWrapper() {
		return Optional.ofNullable(backpackWrapper);
	}

	@Override
	public void tick() {
		//noinspection ConstantConditions - world is always non null at this point
		if (world.isRemote) {
			return;
		}
		backpackWrapper.getUpgradeHandler().getWrappersThatImplement(ITickableUpgrade.class).forEach(upgrade -> upgrade.tick(null, world, getPos(), backpackWrapper));
	}

	@Nonnull
	@Override
	public <T> LazyOptional<T> getCapability(@Nonnull Capability<T> cap, @Nullable Direction side) {
		if (cap == CapabilityItemHandler.ITEM_HANDLER_CAPABILITY) {
			return LazyOptional.of(() -> backpackWrapper.getFilteredHandler()).cast();
		}
		return super.getCapability(cap, side);
	}
}
