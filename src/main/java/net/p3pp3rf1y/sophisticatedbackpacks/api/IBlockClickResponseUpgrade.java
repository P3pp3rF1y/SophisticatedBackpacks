package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.player.Player;

/**
 * Implement if you want your upgrade to be able to respond to PlayerInteractEvent.LeftClickBlock event
 */
public interface IBlockClickResponseUpgrade {
	/**
	 * Gets called when a player left clicks on a block
	 *
	 * @param player - player that clicked the block
	 * @param pos    - position of the block
	 * @return true if the upgrade handled the click and no follow up {@link IBlockClickResponseUpgrade} should be called
	 */
	boolean onBlockClick(Player player, BlockPos pos);
}
