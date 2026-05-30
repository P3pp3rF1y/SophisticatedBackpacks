package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.resources.ResourceLocation;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.ModContainer;
import net.neoforged.fml.ModList;
import net.neoforged.fml.common.Mod;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.fml.event.lifecycle.FMLClientSetupEvent;
import net.neoforged.fml.event.lifecycle.FMLCommonSetupEvent;
import net.neoforged.neoforge.client.gui.ConfigurationScreen;
import net.neoforged.neoforge.client.gui.IConfigScreenFactory;
import net.neoforged.neoforge.client.event.RegisterClientTooltipComponentFactoriesEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.AddReloadListenerEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackShapeReloadListener;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.DatapackBackpackTemplateManager;
import net.p3pp3rf1y.sophisticatedbackpacks.client.ClientEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.KeybindHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.ClientBackpackContentsTooltip;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.ClientMobCatcherHealthTooltip;
import net.p3pp3rf1y.sophisticatedbackpacks.command.SBPCommand;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonEventHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.data.DataGenerators;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModCompat;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.RegistryLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherHealthTooltip;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Mod(SophisticatedBackpacks.MOD_ID)
public class SophisticatedBackpacks {
	public static final String MOD_ID = "sophisticatedbackpacks";
	public static final Logger LOGGER = LogManager.getLogger(MOD_ID);
	private static String networkProtocolVersion;

	private final RegistryLoader registryLoader = new RegistryLoader();
	public final CommonEventHandler commonEventHandler = new CommonEventHandler();

	@SuppressWarnings("java:S1118") //needs to be public for mod to work
	public SophisticatedBackpacks(IEventBus modBus, Dist dist, ModContainer container) {
		networkProtocolVersion = container.getModInfo().getVersion().toString();
		container.registerConfig(ModConfig.Type.SERVER, Config.SERVER_SPEC);
		container.registerConfig(ModConfig.Type.COMMON, Config.COMMON_SPEC);
		if (dist == Dist.CLIENT && !ModList.get().isLoaded("configured")) {
			container.registerExtensionPoint(IConfigScreenFactory.class, ConfigurationScreen::new);
		}
		commonEventHandler.registerHandlers(modBus);
		ModCompat.register();
		if (dist == Dist.CLIENT) {
			ClientEventHandler.registerHandlers(modBus);
			modBus.addListener(KeybindHandler::registerKeyMappings);
			modBus.addListener(SophisticatedBackpacks::registerTooltipComponent);
		}

		modBus.addListener(SophisticatedBackpacks::setup);
		modBus.addListener(DataGenerators::gatherData);
		Config.SERVER.initListeners(modBus);
		modBus.addListener(SophisticatedBackpacks::clientSetup);
		SBPCommand.init(modBus);

		IEventBus eventBus = NeoForge.EVENT_BUS;
		eventBus.addListener(this::onAddReloadListener);
	}

	private static void setup(FMLCommonSetupEvent event) {
		event.enqueueWork(ModItems::registerDispenseBehavior);
		event.enqueueWork(ModItems::registerCauldronInteractions);
	}

	private static void clientSetup(FMLClientSetupEvent event) {
		KeybindHandler.register();
	}

	private static void registerTooltipComponent(RegisterClientTooltipComponentFactoriesEvent event) {
		event.register(BackpackItem.BackpackContentsTooltip.class, ClientBackpackContentsTooltip::new);
		event.register(MobCatcherHealthTooltip.class, ClientMobCatcherHealthTooltip::new);
	}

	private void onAddReloadListener(AddReloadListenerEvent event) {
		event.addListener(registryLoader);
		event.addListener(DatapackBackpackTemplateManager.Loader.INSTANCE);
		event.addListener(BackpackShapeReloadListener.INSTANCE);
	}

	public static ResourceLocation getRL(String regName) {
		return ResourceLocation.fromNamespaceAndPath(MOD_ID, regName);
	}

	public static String getRegistryName(String regName) {
		return MOD_ID + ":" + regName;
	}

	public static String getNetworkProtocolVersion() {
		return networkProtocolVersion;
	}
}
