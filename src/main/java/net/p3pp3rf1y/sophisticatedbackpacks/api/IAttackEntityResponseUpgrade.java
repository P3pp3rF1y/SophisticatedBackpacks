package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.entity.player.PlayerEntity;

/**
 * Implement if you want your upgrade to be able to respond to AttackEntityEvent event
 */
public interface IAttackEntityResponseUpgrade {
	/**
	 * Gets called when a player attacks an entity
	 *
	 * @param player - player that's attacking entity
	 * @return true if the upgrade handled the attack and no follow up {@link IAttackEntityResponseUpgrade} should be called
	 */
	boolean onAttackEntity(PlayerEntity player);
}
