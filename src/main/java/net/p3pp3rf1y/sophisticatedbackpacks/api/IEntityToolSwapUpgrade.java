package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.world.World;

/**
 * Implement if you want your upgrade to respond to key press to swap appropriate tool into player's hand based on entity they are looking at
 */
public interface IEntityToolSwapUpgrade {
	/**
	 * Just a simple flag whether the upgrade can actually process interactions. Used for different levels of the same upgrade where one level can and the other can't interact.
	 *
	 * @return true if the onBlockInteract should be run otherwise false
	 */
	default boolean canProcessEntityInteract() {
		return true;
	}

	/**
	 * Called when player presses tool swap keybind over an entity
	 *
	 * @param world  World
	 * @param entity Entity player is looking at
	 * @param player Player that pressed the key
	 * @return true if the keypress was handled by this upgrade and no other {@link IEntityToolSwapUpgrade} upgrades should process their logic
	 */
	boolean onEntityInteract(World world, Entity entity, PlayerEntity player);
}
