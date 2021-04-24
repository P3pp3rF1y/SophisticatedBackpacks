package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import net.minecraft.item.ItemStack;
import net.minecraftforge.common.ToolType;

class ToolTypeMatcher implements CacheableStackPredicate {
	private final ToolType toolType;

	public ToolTypeMatcher(ToolType toolType) {
		this.toolType = toolType;
	}

	@Override
	public boolean test(ItemStack stack) {
		return stack.getToolTypes().contains(toolType);
	}

	@Override
	public boolean preventsCaching(ItemStack stack) {
		return true;
	}
}
