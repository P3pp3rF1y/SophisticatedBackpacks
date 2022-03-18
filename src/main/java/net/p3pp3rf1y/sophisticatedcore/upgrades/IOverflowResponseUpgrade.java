package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.util.ItemStackHelper;

public interface IOverflowResponseUpgrade {
	default boolean stackMatchesFilterStack(ItemStack stack, ItemStack filterStack) {
		if (stack.getItem() != filterStack.getItem()) {
			return false;
		}

		if (getFilterLogic().getPrimaryMatch() == PrimaryMatch.TAGS) {
			return true;
		}

		if (getFilterLogic().shouldMatchDurability() && stack.getDamageValue() != filterStack.getDamageValue()) {
			return false;
		}

		return !getFilterLogic().shouldMatchNbt() || ItemStackHelper.areItemStackTagsEqualIgnoreDurability(stack, filterStack);
	}

	FilterLogic getFilterLogic();

	boolean worksInGui();

	ItemStack onOverflow(ItemStack stack);

	boolean stackMatchesFilter(ItemStack stack);
}
