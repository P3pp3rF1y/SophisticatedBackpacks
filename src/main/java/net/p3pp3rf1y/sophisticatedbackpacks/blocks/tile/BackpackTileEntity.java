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
import net.p3pp3rf1y.sophisticatedbackpacks.util.InjectionHelper;

import java.util.Optional;

public class BackpackTileEntity extends TileEntity implements ITickableTileEntity {
	@ObjectHolder(SophisticatedBackpacks.MOD_ID + ":backpack")
	public static final TileEntityType<BackpackTileEntity> TYPE = InjectionHelper.nullValue();

	private BackpackWrapper backpackWrapper;
	private boolean persistentSet = false;

	public BackpackTileEntity() {
		super(TYPE);
	}

	public void setBackpack(BackpackWrapper backpackWrapper) {
		this.backpackWrapper = backpackWrapper;
	}

	@Override
	public void read(BlockState state, CompoundNBT nbt) {
		super.read(state, nbt);
		backpackWrapper = new BackpackWrapper(ItemStack.read(nbt.getCompound("backpackData")));
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

	public Optional<BackpackWrapper> getBackpackWrapper() {
		return Optional.ofNullable(backpackWrapper);
	}

	@Override
	public void tick() {
		setPersistent();
	}

	private void setPersistent() {
		if (!persistentSet && !world.isRemote && backpackWrapper != null) {
			persistentSet = true;
			backpackWrapper.setPersistent(this);
		}
	}
}
