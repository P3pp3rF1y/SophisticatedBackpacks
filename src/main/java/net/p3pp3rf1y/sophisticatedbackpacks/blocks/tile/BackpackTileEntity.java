package net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile;

import net.minecraft.block.BlockState;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;

import java.util.Optional;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.BACKPACK_TILE_TYPE;

public class BackpackTileEntity extends TileEntity implements ITickableTileEntity {

	private IBackpackWrapper backpackWrapper;

	public BackpackTileEntity() {
		super(BACKPACK_TILE_TYPE.get());
	}

	public void setBackpack(ItemStack backpack) {
		backpackWrapper = new BackpackWrapper(backpack, this);
	}

	@Override
	public void read(BlockState state, CompoundNBT nbt) {
		super.read(state, nbt);
		backpackWrapper = new BackpackWrapper(ItemStack.read(nbt.getCompound("backpackData")), this);
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
		//noop for now
	}
}
