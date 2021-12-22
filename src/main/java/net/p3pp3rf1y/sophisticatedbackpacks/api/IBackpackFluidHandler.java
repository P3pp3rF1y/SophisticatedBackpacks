package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.tags.Tag;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;

public interface IBackpackFluidHandler extends IFluidHandlerItem {
	int fill(FluidStack resource, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(Tag<Fluid> resourceTag, int maxDrain, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(FluidStack resource, FluidAction action, boolean ignoreInOutLimit);

	FluidStack drain(int maxDrain, FluidAction action, boolean ignoreInOutLimit);
}
