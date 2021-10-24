package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pump;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.level.material.Fluids;
import net.minecraftforge.fluids.FluidAttributes;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;

import java.util.function.Supplier;

public class FluidFilterContainer {
	private final Player player;
	private final IServerUpdater serverUpdater;
	private final Supplier<FluidFilterLogic> fluidFilterLogic;
	private static final String DATA_FLUID = "setFluid";

	public FluidFilterContainer(Player player, IServerUpdater serverUpdater, Supplier<FluidFilterLogic> fluidFilterLogic) {
		this.player = player;
		this.serverUpdater = serverUpdater;
		this.fluidFilterLogic = fluidFilterLogic;
	}

	public Fluid getFluid(int index) {
		return fluidFilterLogic.get().getFluid(index);
	}

	private void setFluid(int index, Fluid fluid) {
		fluidFilterLogic.get().setFluid(index, fluid);
		serverUpdater.sendDataToServer(() -> serializeSetFluidData(index, fluid));
	}

	private CompoundTag serializeSetFluidData(int index, Fluid fluid) {
		CompoundTag ret = new CompoundTag();
		CompoundTag fluidNbt = new CompoundTag();
		fluidNbt.putInt("index", index);
		//noinspection ConstantConditions
		fluidNbt.putString("fluid", fluid.getRegistryName().toString());
		ret.put(DATA_FLUID, fluidNbt);
		return ret;
	}

	public boolean handleMessage(CompoundTag data) {
		if (data.contains(DATA_FLUID)) {
			CompoundTag fluidData = data.getCompound(DATA_FLUID);
			Fluid fluid = ForgeRegistries.FLUIDS.getValue(new ResourceLocation(fluidData.getString("fluid")));
			if (fluid != null) {
				setFluid(fluidData.getInt("index"), fluid);
			}
			return true;
		}
		return false;
	}

	public int getNumberOfFluidFilters() {
		return fluidFilterLogic.get().getNumberOfFluidFilters();
	}

	public void slotClick(int index) {
		ItemStack carried = player.containerMenu.getCarried();
		if (carried.isEmpty()) {
			setFluid(index, Fluids.EMPTY);
			return;
		}

		carried.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).ifPresent(itemFluidHandler -> {
			FluidStack containedFluid = itemFluidHandler.drain(FluidAttributes.BUCKET_VOLUME, IFluidHandler.FluidAction.SIMULATE);
			if (!containedFluid.isEmpty()) {
				setFluid(index, containedFluid.getRawFluid());
			}
		});
	}
}
