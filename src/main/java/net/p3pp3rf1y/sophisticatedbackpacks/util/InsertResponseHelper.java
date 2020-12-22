package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;

import java.util.List;

public class InsertResponseHelper {
	private InsertResponseHelper() {}

	public static void runOnAfterInsert(int slot, boolean simulate, IItemHandler handler, ItemStack backpack) {
		if (!simulate) {
			backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
					.ifPresent(wrapper -> wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class)
							.forEach(u -> u.onAfterInsert(handler, slot)));
		}
	}

	public static ItemStack runOnBeforeInsert(int slot, ItemStack stack, boolean simulate, IItemHandler handler, ItemStack backpack) {
		return backpack.getCapability(BackpackWrapper.BACKPACK_WRAPPER_CAPABILITY)
				.map(wrapper -> {
					List<IInsertResponseUpgrade> wrappers = wrapper.getUpgradeHandler().getWrappersThatImplement(IInsertResponseUpgrade.class);
					ItemStack remaining = stack;
					for (IInsertResponseUpgrade upgrade : wrappers) {
						remaining = upgrade.onBeforeInsert(handler, slot, remaining, simulate);
						if (remaining.isEmpty()) {
							return ItemStack.EMPTY;
						}
					}
					return remaining;
				}).orElse(stack);
	}
}
