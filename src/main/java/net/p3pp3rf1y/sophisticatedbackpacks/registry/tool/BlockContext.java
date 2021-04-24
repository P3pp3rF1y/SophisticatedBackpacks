package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

class BlockContext {
	private final World world;

	private final BlockState state;
	private final Block block;
	private final BlockPos pos;

	public BlockContext(World world, BlockState state, Block block, BlockPos pos) {
		this.world = world;
		this.state = state;
		this.block = block;
		this.pos = pos;
	}

	public World getWorld() {
		return world;
	}

	public BlockState getState() {
		return state;
	}

	public Block getBlock() {
		return block;
	}

	public BlockPos getPos() {
		return pos;
	}
}
