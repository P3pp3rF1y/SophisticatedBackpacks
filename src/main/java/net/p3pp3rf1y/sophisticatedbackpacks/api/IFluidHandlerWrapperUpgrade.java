package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;

public interface IFluidHandlerWrapperUpgrade {
	@Nullable
	IBackpackFluidHandler wrapHandler(@Nullable IBackpackFluidHandler fluidHandler, ItemStack backpack);
}
