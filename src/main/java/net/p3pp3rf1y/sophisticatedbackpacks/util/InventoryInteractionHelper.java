package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraft.item.ItemUseContext;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IItemHandlerInteractionUpgrade;

import java.util.List;

public class InventoryInteractionHelper {
	private InventoryInteractionHelper() {}

	public static boolean tryInventoryInteraction(ItemUseContext context) {

		return tryInventoryInteraction(context.getPos(), context.getWorld(), context.getItem(), context.getFace());
	}

	public static Boolean tryInventoryInteraction(BlockPos pos, World world, ItemStack backpack, Direction face) {
		return WorldHelper.getTile(world, pos)
				.map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, face)
						.map(itemHandler -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
								.map(wrapper -> tryRunningInteractionWrappers(itemHandler, wrapper))
								.orElse(false)).orElse(false)
				).orElse(false);
	}

	private static boolean tryRunningInteractionWrappers(net.minecraftforge.items.IItemHandler itemHandler, IBackpackWrapper wrapper) {
		List<IItemHandlerInteractionUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IItemHandlerInteractionUpgrade.class);
		if (wrappers.isEmpty()) {
			return false;
		}
		wrappers.forEach(upgrade -> upgrade.onHandlerInteract(itemHandler));
		return true;
	}
}
