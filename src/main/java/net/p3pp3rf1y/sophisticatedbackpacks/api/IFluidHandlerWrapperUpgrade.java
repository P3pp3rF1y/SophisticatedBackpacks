package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.item.ItemStack;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;

import javax.annotation.Nullable;

public interface IFluidHandlerWrapperUpgrade {
	@Nullable
	IFluidHandlerItem wrapHandler(@Nullable IFluidHandlerItem fluidHandler, ItemStack backpack);
}
