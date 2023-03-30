package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.List;

public class InventoryInteractionHelper {
	private InventoryInteractionHelper() {}

	public static boolean tryInventoryInteraction(UseOnContext context) {
		Player player = context.getPlayer();
		if (player == null) {
			return false;
		}
		return tryInventoryInteraction(context.getClickedPos(), context.getLevel(), context.getItemInHand(), context.getClickedFace(), player);
	}

	public static boolean tryInventoryInteraction(BlockPos pos, Level world, ItemStack backpack, Direction face, Player player) {
		if (Config.SERVER.noInteractionBlocks.isBlockInteractionDisallowed(world.getBlockState(pos).getBlock())) {
			return false;
		}

		return WorldHelper.getBlockEntity(world, pos)
				.map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, face)
						.map(itemHandler -> player.level.isClientSide || backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
								.map(wrapper -> tryRunningInteractionWrappers(itemHandler, wrapper, player))
								.orElse(false)).orElse(false)
				).orElse(false);
	}

	private static boolean tryRunningInteractionWrappers(IItemHandler itemHandler, IStorageWrapper wrapper, Player player) {
		List<IItemHandlerInteractionUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IItemHandlerInteractionUpgrade.class);
		if (wrappers.isEmpty()) {
			return false;
		}
		wrappers.forEach(upgrade -> upgrade.onHandlerInteract(itemHandler, player));
		return true;
	}
}
