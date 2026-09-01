package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.UseOnContext;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemResourceHandlerInteractionUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.CapabilityHelper;

import java.util.List;

public class InventoryInteractionHelper {
	private InventoryInteractionHelper() {
	}

	public static boolean tryInventoryInteraction(UseOnContext context) {
		Player player = context.getPlayer();
		if (player == null) {
			return false;
		}
		return tryInventoryInteraction(context.getClickedPos(), context.getLevel(), context.getItemInHand(), context.getClickedFace(), player);
	}

	public static boolean tryInventoryInteraction(BlockPos pos, Level level, ItemStack backpack, Direction face, Player player) {
		if (Config.SERVER.noInteractionBlocks.isBlockInteractionDisallowed(level.getBlockState(pos).getBlock())) {
			return false;
		}

		return CapabilityHelper.getFromItemHandler(level, pos, face,
				itemHandler -> player.level().isClientSide()
						|| tryRunningInteractionWrappers(itemHandler, BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, backpack), player),
				false);
	}

	private static boolean tryRunningInteractionWrappers(ResourceHandler<ItemResource> itemHandler, IStorageWrapper wrapper, Player player) {
		List<IItemResourceHandlerInteractionUpgrade> wrappers = wrapper.getUpgradeHandler()
				.getWrappersThatImplement(IItemResourceHandlerInteractionUpgrade.class);
		if (wrappers.isEmpty()) {
			return false;
		}
		wrappers.forEach(upgrade -> upgrade.onHandlerInteract(itemHandler, player));
		return true;
	}
}
