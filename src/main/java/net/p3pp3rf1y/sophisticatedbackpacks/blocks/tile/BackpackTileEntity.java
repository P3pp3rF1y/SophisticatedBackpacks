package net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile;

import net.minecraft.block.BlockState;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.tileentity.ITickableTileEntity;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.registries.ObjectHolder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import java.util.Optional;

public class BackpackTileEntity extends TileEntity implements ITickableTileEntity {
	@ObjectHolder(SophisticatedBackpacks.MOD_ID + ":backpack")
	public static final TileEntityType<BackpackTileEntity> TYPE = RegistryHelper.nullValue();

	private IBackpackWrapper IBackpackWrapper;

	public BackpackTileEntity() {
		super(TYPE);
	}

	public void setBackpack(ItemStack backpack) {
		IBackpackWrapper = new BackpackWrapper(backpack, this);
	}

	@Override
	public void read(BlockState state, CompoundNBT nbt) {
		super.read(state, nbt);
		IBackpackWrapper = new BackpackWrapper(ItemStack.read(nbt.getCompound("backpackData")), this);
	}

	@Override
	public CompoundNBT write(CompoundNBT compound) {
		CompoundNBT ret = super.write(compound);
		writeBackpack(ret);
		return ret;
	}

	private void writeBackpack(CompoundNBT ret) {
		ret.put("backpackData", IBackpackWrapper.getBackpack().write(new CompoundNBT()));
	}

	@Override
	public CompoundNBT getUpdateTag() {
		CompoundNBT ret = super.getUpdateTag();
		writeBackpack(ret);
		return ret;
	}

	public Optional<IBackpackWrapper> getBackpackWrapper() {
		return Optional.ofNullable(IBackpackWrapper);
	}

	@Override
	public void tick() {
		//noop for now
	}
}
