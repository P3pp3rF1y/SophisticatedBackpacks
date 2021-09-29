package net.p3pp3rf1y.sophisticatedbackpacks.client;

import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.renderer.ItemBlockRenderTypes;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.ItemEntityRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.ReloadableResourceManager;
import net.minecraft.server.packs.resources.ResourceManagerReloadListener;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.client.event.ClientPlayerNetworkEvent;
import net.minecraftforge.client.event.EntityRenderersEvent;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.event.ParticleFactoryRegisterEvent;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.client.model.ModelLoaderRegistry;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModBlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModItemColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackBlockEntityRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackDynamicModel;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackModel;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackTooltipRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.BackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY;

public class ClientEventHandler {
	private ClientEventHandler() {}

	private static final String BACKPACK_REG_NAME = "backpack";
	public static final ModelLayerLocation BACKPACK_LAYER = new ModelLayerLocation(new ResourceLocation(SophisticatedBackpacks.MOD_ID, BACKPACK_REG_NAME), "main");

	public static void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(ClientEventHandler::loadComplete);
		modBus.addListener(ClientEventHandler::clientSetup);
		modBus.addListener(ClientEventHandler::stitchTextures);
		modBus.addListener(ClientEventHandler::onModelRegistry);
		modBus.addListener(ClientEventHandler::registerLayer);
		modBus.addListener(ClientEventHandler::registerEntityRenderers);
		modBus.addListener(ClientEventHandler::registerReloadListener);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(ClientEventHandler::onPlayerJoinServer);
		eventBus.addListener(BackpackTooltipRenderer::renderBackpackTooltip);
		eventBus.addListener(BackpackTooltipRenderer::onWorldLoad);
		eventBus.addListener(BackpackSoundHandler::tick);
		eventBus.addListener(BackpackSoundHandler::onWorldUnload);
	}

	private static void loadComplete(FMLLoadCompleteEvent event) {
		event.enqueueWork(() -> {
			ModItemColors.init();
			ModBlockColors.init();
		});
	}

	private static void onModelRegistry(ModelRegistryEvent event) {
		ModelLoaderRegistry.registerLoader(RegistryHelper.getRL(BACKPACK_REG_NAME), BackpackDynamicModel.Loader.INSTANCE);
	}

	private static void clientSetup(FMLClientSetupEvent event) {
		ItemBlockRenderTypes.setRenderLayer(ModBlocks.BACKPACK.get(), RenderType.cutout());
		ItemBlockRenderTypes.setRenderLayer(ModBlocks.IRON_BACKPACK.get(), RenderType.cutout());
		ItemBlockRenderTypes.setRenderLayer(ModBlocks.GOLD_BACKPACK.get(), RenderType.cutout());
		ItemBlockRenderTypes.setRenderLayer(ModBlocks.DIAMOND_BACKPACK.get(), RenderType.cutout());
		ItemBlockRenderTypes.setRenderLayer(ModBlocks.NETHERITE_BACKPACK.get(), RenderType.cutout());
	}

	public static void registerReloadListener(ParticleFactoryRegisterEvent event) {
		((ReloadableResourceManager) Minecraft.getInstance().getResourceManager()).registerReloadListener((ResourceManagerReloadListener) resourceManager -> registerBackpackLayer());
	}

	private static void registerEntityRenderers(EntityRenderersEvent.RegisterRenderers event) {
		event.registerEntityRenderer(EVERLASTING_BACKPACK_ITEM_ENTITY.get(), ItemEntityRenderer::new);
		event.registerBlockEntityRenderer(ModBlocks.BACKPACK_TILE_TYPE.get(), BackpackBlockEntityRenderer::new);
	}

	public static void registerLayer(EntityRenderersEvent.RegisterLayerDefinitions event) {
		event.registerLayerDefinition(BACKPACK_LAYER, BackpackModel::createBodyLayer);
	}

	@SuppressWarnings("java:S3740") //explanation below
	private static void registerBackpackLayer() {
		EntityRenderDispatcher renderManager = Minecraft.getInstance().getEntityRenderDispatcher();
		Map<String, EntityRenderer<? extends Player>> skinMap = renderManager.getSkinMap();
		for (EntityRenderer<? extends Player> renderer : skinMap.values()) {
			if (renderer instanceof LivingEntityRenderer livingEntityRenderer) {
				//noinspection rawtypes ,unchecked - this is not going to fail as the LivingRenderer makes sure the types are right, but there doesn't seem to be a way to us inference here
				livingEntityRenderer.addLayer(new BackpackLayerRenderer(livingEntityRenderer));
			}
		}
		renderManager.renderers.forEach((e, r) -> {
			if (r instanceof LivingEntityRenderer livingEntityRenderer) {
				//noinspection rawtypes ,unchecked - this is not going to fail as the LivingRenderer makes sure the types are right, but there doesn't seem to be a way to us inference here
				livingEntityRenderer.addLayer(new BackpackLayerRenderer(livingEntityRenderer));
			}
		});
	}

	public static void stitchTextures(TextureStitchEvent.Pre evt) {
		if (evt.getMap().location() == InventoryMenu.BLOCK_ATLAS) {
			evt.addSprite(BackpackContainer.EMPTY_UPGRADE_SLOT_BACKGROUND);
			evt.addSprite(TankUpgradeContainer.EMPTY_TANK_INPUT_SLOT_BACKGROUND);
			evt.addSprite(TankUpgradeContainer.EMPTY_TANK_OUTPUT_SLOT_BACKGROUND);
			evt.addSprite(BatteryUpgradeContainer.EMPTY_BATTERY_INPUT_SLOT_BACKGROUND);
			evt.addSprite(BatteryUpgradeContainer.EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND);
		}
	}

	private static void onPlayerJoinServer(ClientPlayerNetworkEvent.LoggedInEvent evt) {
		//noinspection ConstantConditions - by the time player is joining the world is not null
		RecipeHelper.setWorld(Minecraft.getInstance().level);
	}
}
