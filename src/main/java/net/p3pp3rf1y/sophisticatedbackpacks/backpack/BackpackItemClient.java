package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.client.Minecraft;
import net.minecraft.world.inventory.tooltip.TooltipComponent;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;

public class BackpackItemClient {
	@Nullable
	public static TooltipComponent getTooltipImage(ItemStack stack) {
		Minecraft mc = Minecraft.getInstance();
		if (mc.hasShiftDown() || (mc.player != null && !mc.player.containerMenu.getCarried().isEmpty())) {
			return new BackpackItem.BackpackContentsTooltip(stack);
		}
		return null;
	}
}
