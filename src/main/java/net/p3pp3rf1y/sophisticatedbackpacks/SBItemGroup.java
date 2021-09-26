package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

public class SBItemGroup extends CreativeModeTab {
	private ItemStack tabIcon;

	SBItemGroup() {
		super(SophisticatedBackpacks.MOD_ID);
	}

	@Override
	public ItemStack makeIcon() {
		if (tabIcon == null) {
			tabIcon = new ItemStack(ModItems.BACKPACK.get());
		}
		return tabIcon;
	}
}
