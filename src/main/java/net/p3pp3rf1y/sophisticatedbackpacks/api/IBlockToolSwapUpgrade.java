package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.block.BlockState;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;

/**
 * Implement if you want your upgrade to respond to key press to swap appropriate tool into player's hand based on block they are looking at
 */
public interface IBlockToolSwapUpgrade {
	/**
	 * Just a simple flag whether the upgrade can actually process interactions. Used for different levels of the same upgrade where one level can and the other can't interact.
	 *
	 * @return true if the onBlockInteract should be run otherwise false
	 */
	default boolean canProcessBlockInteract() {
		return true;
	}

	/**
	 * Called when player presses tool swap keybind over a block
	 *
	 * @param world      World
	 * @param pos        Position of the block
	 * @param blockState Its blockstate
	 * @param player     Player that pressed the key
	 * @return true if the keypress was handled by this upgrade and no other {@link IBlockToolSwapUpgrade} upgrades should process their logic
	 */
	boolean onBlockInteract(World world, BlockPos pos, BlockState blockState, PlayerEntity player);
}
