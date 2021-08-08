package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;

import java.util.List;

public class InventoryInteractionHelper {
	private InventoryInteractionHelper() {}

	public static boolean tryInventoryInteraction(ItemUseContext context) {
		PlayerEntity player = context.getPlayer();
		if (player == null) {
			return false;
		}
		return tryInventoryInteraction(context.getClickedPos(), context.getLevel(), context.getItemInHand(), context.getClickedFace(), player);
	}

	public static boolean tryInventoryInteraction(BlockPos pos, World world, ItemStack backpack, Direction face, PlayerEntity player) {
		return WorldHelper.getTile(world, pos)
				.map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, face)
						.map(itemHandler -> player.level.isClientSide || backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
								.map(wrapper -> tryRunningInteractionWrappers(itemHandler, wrapper, player))
								.orElse(false)).orElse(false)
				).orElse(false);
	}

	private static boolean tryRunningInteractionWrappers(IItemHandler itemHandler, IBackpackWrapper wrapper, PlayerEntity player) {
		List<IItemHandlerInteractionUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IItemHandlerInteractionUpgrade.class);
		if (wrappers.isEmpty()) {
			return false;
		}
		wrappers.forEach(upgrade -> upgrade.onHandlerInteract(itemHandler, player));
		return true;
	}
}
