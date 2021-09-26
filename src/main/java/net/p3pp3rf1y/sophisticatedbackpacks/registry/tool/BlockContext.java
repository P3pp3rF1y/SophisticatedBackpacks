package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import net.minecraft.core.BlockPos;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;

class BlockContext {
	private final Level world;

	private final BlockState state;
	private final Block block;
	private final BlockPos pos;

	public BlockContext(Level world, BlockState state, Block block, BlockPos pos) {
		this.world = world;
		this.state = state;
		this.block = block;
		this.pos = pos;
	}

	public Level getWorld() {
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
