package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.level.Level;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.client.MinecraftForgeClient;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.AddReloadListenerEvent;
import net.minecraftforge.event.RegisterCommandsEvent;
import net.minecraftforge.event.server.ServerStartedEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.ModLoadingContext;
import net.minecraftforge.fml.common.Mod;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLCommonSetupEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.fml.loading.FMLEnvironment;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.ClientEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.KeybindHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.ClientBackpackContentsTooltip;
import net.p3pp3rf1y.sophisticatedbackpacks.command.SBPCommand;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.data.DataGenerators;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModCompat;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModLoot;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.RegistryLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(SophisticatedBackpacks.MOD_ID)
public class SophisticatedBackpacks {
	public static final String MOD_ID = "sophisticatedbackpacks";
	public static final Logger LOGGER = LogManager.getLogger(MOD_ID);

	public static final CreativeModeTab ITEM_GROUP = new SBItemGroup();

	private final RegistryLoader registryLoader = new RegistryLoader();
	public final CommonEventHandler commonEventHandler = new CommonEventHandler();

	@SuppressWarnings("java:S1118") //needs to be public for mod to work
	public SophisticatedBackpacks() {
		ModLoadingContext.get().registerConfig(ModConfig.Type.COMMON, Config.COMMON_SPEC);
		ModLoadingContext.get().registerConfig(ModConfig.Type.CLIENT, Config.CLIENT_SPEC);
		commonEventHandler.registerHandlers();
		if (FMLEnvironment.dist == Dist.CLIENT) {
			ClientEventHandler.registerHandlers();
		}

		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(SophisticatedBackpacks::setup);
		modBus.addListener(DataGenerators::gatherData);
		modBus.addListener(Config.COMMON::onConfigReload);
		modBus.addListener(CapabilityBackpackWrapper::onRegister);
		modBus.addListener(SophisticatedBackpacks::clientSetup);
		ModLoot.init();

		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(SophisticatedBackpacks::serverStarted);
		eventBus.addListener(SophisticatedBackpacks::registerCommands);
		eventBus.addListener(this::onAddReloadListener);
	}

	private static void setup(FMLCommonSetupEvent event) {
		PacketHandler.init();
		ModCompat.initCompats();
		ModItems.registerDispenseBehavior();
		ModItems.registerCauldronInteractions();
		SBPCommand.registerArgumentTypes();
	}

	private static void clientSetup(FMLClientSetupEvent event) {
		KeybindHandler.register(event);
		MinecraftForgeClient.registerTooltipComponentFactory(BackpackItem.BackpackContentsTooltip.class, ClientBackpackContentsTooltip::new);
	}

	private static void serverStarted(ServerStartedEvent event) {
		ServerLevel world = event.getServer().getLevel(Level.OVERWORLD);
		if (world != null) {
			RecipeHelper.setWorld(world);
		}
	}

	private static void registerCommands(RegisterCommandsEvent event) {
		SBPCommand.register(event.getDispatcher());
	}

	private void onAddReloadListener(AddReloadListenerEvent event) {
		event.addListener(registryLoader);
	}
}
