package net.p3pp3rf1y.sophisticatedcore;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLEnvironment;
import net.p3pp3rf1y.sophisticatedcore.init.ModCompat;
import net.p3pp3rf1y.sophisticatedcore.client.ClientEventHandler;
import net.p3pp3rf1y.sophisticatedcore.common.CommonEventHandler;
import net.p3pp3rf1y.sophisticatedcore.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedcore.data.DataGenerators;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(SophisticatedCore.MOD_ID)
public class SophisticatedCore {
	public static final String MOD_ID = "sophisticatedcore";
	public static final Logger LOGGER = LogManager.getLogger(MOD_ID);
	public static final PacketHandler PACKET_HANDLER = new PacketHandler(MOD_ID);

	public final CommonEventHandler commonEventHandler = new CommonEventHandler();

	@SuppressWarnings("java:S1118") //needs to be public for mod to work
	public SophisticatedCore() {
		ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.COMMON_SPEC);
		ModLoadingContext.get().registerConfig(ModConfig.Type.CLIENT, Config.CLIENT_SPEC);
		commonEventHandler.registerHandlers();
		if (FMLEnvironment.dist == Dist.CLIENT) {
			ClientEventHandler.registerHandlers();
		}
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(SophisticatedCore::setup);
		modBus.addListener(DataGenerators::gatherData);
		modBus.addGenericListener(RecipeSerializer.class, this::registerRecipeSerializers);
	}

	private void registerRecipeSerializers(RegistryEvent.Register<RecipeSerializer<?>> evt) {
		evt.getRegistry().register(UpgradeNextTierRecipe.SERIALIZER.setRegistryName(MOD_ID, "upgrade_next_tier"));
	}

	private static void setup(FMLCommonSetupEvent event) {
		PACKET_HANDLER.init();
		ModCompat.initCompats();
	}

	public static ResourceLocation getRL(String regName) {
		return new ResourceLocation(getRegistryName(regName));
	}

	public static String getRegistryName(String regName) {
		return MOD_ID + ":" + regName;
	}
}
