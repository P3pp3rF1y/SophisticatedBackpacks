package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.fluid.Fluid;
import net.minecraft.tags.ITag;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;

public interface IBackpackFluidHandler extends IFluidHandlerItem {
	int fill(FluidStack resource, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(ITag<Fluid> resourceTag, int maxDrain, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(FluidStack resource, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(int maxDrain, FluidAction action, boolean ignoreInOutLimit);
}
