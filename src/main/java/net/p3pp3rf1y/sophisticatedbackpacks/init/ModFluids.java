package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.fluid.FlowingFluid;
import net.minecraft.fluid.Fluid;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeTagHandler;
import net.minecraftforge.common.Tags;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fluids.FluidAttributes;
import net.minecraftforge.fluids.ForgeFlowingFluid;
import net.minecraftforge.fml.RegistryObject;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class ModFluids {
	private ModFluids() {}

	public static final ResourceLocation EXPERIENCE_TAG_NAME = new ResourceLocation("forge:experience");

	public static final Tags.IOptionalNamedTag<Fluid> EXPERIENCE_TAG = ForgeTagHandler.createOptionalTag(ForgeRegistries.FLUIDS, EXPERIENCE_TAG_NAME);

	private static final ResourceLocation XP_STILL_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "fluids/xp_still");
	private static final ResourceLocation XP_FLOWING_TEXTURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, "fluids/xp_flowing");

	public static final DeferredRegister<Fluid> FLUIDS = DeferredRegister.create(ForgeRegistries.FLUIDS, SophisticatedBackpacks.MOD_ID);

	public static final RegistryObject<FlowingFluid> XP_STILL = FLUIDS.register("xp_still", () -> new ForgeFlowingFluid.Source(ModFluids.XP_PROPERTIES));
	public static final RegistryObject<FlowingFluid> XP_FLOWING = FLUIDS.register("xp_flowing", () -> new ForgeFlowingFluid.Flowing(ModFluids.XP_PROPERTIES));

	public static final ForgeFlowingFluid.Properties XP_PROPERTIES = new ForgeFlowingFluid.Properties(XP_STILL, XP_FLOWING, FluidAttributes.builder(XP_STILL_TEXTURE, XP_FLOWING_TEXTURE).luminosity(10).density(800).viscosity(1500));

	public static void registerHandlers(IEventBus modBus) {
		FLUIDS.register(modBus);
	}
}
