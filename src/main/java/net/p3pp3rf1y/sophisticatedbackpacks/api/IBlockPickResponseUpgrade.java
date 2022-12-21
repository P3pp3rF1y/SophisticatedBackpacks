package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;

public interface IBlockPickResponseUpgrade {
	boolean pickBlock(Player player, ItemStack filter);
}
