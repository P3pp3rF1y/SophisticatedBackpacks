package net.p3pp3rf1y.sophisticatedbackpacks.blocks;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.SoundType;
import net.minecraft.block.material.Material;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.IBlockReader;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.blocks.tile.BackpackTileEntity;

import javax.annotation.Nullable;

public class BackpackBlock extends Block {
	private static final VoxelShape SHAPE = makeCuboidShape(2, 0, 2, 14, 12, 14);

	public BackpackBlock(String regName) {
		super(Properties.create(Material.WOOL).notSolid().hardnessAndResistance(0.8F).sound(SoundType.CLOTH));
		setRegistryName(SophisticatedBackpacks.MOD_ID, regName);
	}

	@Override
	public VoxelShape getShape(BlockState state, IBlockReader worldIn, BlockPos pos, ISelectionContext context) {
		return SHAPE;
	}

	@Override
	public boolean hasTileEntity(BlockState state) {
		return true;
	}

	@Nullable
	@Override
	public TileEntity createTileEntity(BlockState state, IBlockReader world) {
		return new BackpackTileEntity();
	}
}
