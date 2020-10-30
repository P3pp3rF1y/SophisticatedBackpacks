package net.p3pp3rf1y.sophisticatedbackpacks.blocks;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.block.material.Material;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.shapes.ISelectionContext;
import net.minecraft.util.math.shapes.VoxelShape;
import net.minecraft.world.IBlockReader;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class BackpackBlock extends Block {
	private static final VoxelShape SHAPE = makeCuboidShape(2, 0, 2, 14, 12, 14);

	public BackpackBlock(String regName) {
		super(Properties.create(Material.WOOL).setOpaque((s, r, p) -> false).notSolid());
		setRegistryName(SophisticatedBackpacks.MOD_ID, regName);
	}

	@Override
	public VoxelShape getShape(BlockState state, IBlockReader worldIn, BlockPos pos, ISelectionContext context) {
		return SHAPE;
	}
}
