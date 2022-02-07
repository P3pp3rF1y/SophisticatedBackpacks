package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;

import javax.annotation.Nullable;

public interface IFluidHandlerWrapperUpgrade {
	@Nullable
	IStorageFluidHandler wrapHandler(@Nullable IStorageFluidHandler fluidHandler, ItemStack backpack);
}
