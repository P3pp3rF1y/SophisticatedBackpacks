package net.p3pp3rf1y.sophisticatedbackpacks.client;

import com.mojang.math.Axis;
import net.minecraft.client.Minecraft;
import net.minecraft.client.model.geom.ModelLayerLocation;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.client.renderer.entity.EntityRenderDispatcher;
import net.minecraft.client.renderer.entity.EntityRenderer;
import net.minecraft.client.renderer.entity.ItemEntityRenderer;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.client.renderer.entity.state.LivingEntityRenderState;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.Identifier;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.ResourceManagerReloadListener;
import net.minecraft.world.entity.Avatar;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.player.PlayerModelType;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.HitResult;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.client.event.*;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.client.renderstate.RegisterRenderStateModifiersEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.neoforged.neoforge.event.tick.EntityTickEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackShapes;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplateStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.BackpackTintSources;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModBlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.*;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BlockPickPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestPlayerSettingsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeClientTickHandler;
import net.p3pp3rf1y.sophisticatedcore.client.render.UpgradeClientRegistry;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeClientData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderDataHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeClientDataType;
import org.joml.Vector3f;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY;

public class ClientEventHandler {
	private ClientEventHandler() {
	}

	private static final String BACKPACK_REG_NAME = "backpack";
	public static final ModelLayerLocation BACKPACK_LAYER = new ModelLayerLocation(
			Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, BACKPACK_REG_NAME), "main");

	public static void registerHandlers(IEventBus modBus) {
		modBus.addListener(ClientEventHandler::onModelRegistry);
		modBus.addListener(ClientEventHandler::registerEntityRenderers);
		modBus.addListener(ClientEventHandler::registerReloadListener);
		modBus.addListener(BackpackTintSources::register);
		modBus.addListener(ModBlockColors::registerBlockColorHandlers);
		modBus.addListener(ClientEventHandler::registerBackpackEntityRenderStateModifier);
		modBus.addListener(ClientEventHandler::registerBackpackItemModels);
		modBus.addListener(ClientEventHandler::registerBlockStateModels);
		BackpackShapes.setShapeProvider(ClientBackpackShapeProvider.INSTANCE);
		IEventBus eventBus = NeoForge.EVENT_BUS;
		eventBus.addListener(ClientBackpackContentsTooltip::onWorldLoad);
		eventBus.addListener(ClientEventHandler::handleBlockPick);
		eventBus.addListener(ClientEventHandler::onPlayerLoggingIn);
		eventBus.addListener(ClientEventHandler::onPlayerLoggingOut);
		eventBus.addListener(ClientEventHandler::submitCustomGeometry);
		eventBus.addListener(BackpackStorage::onClientWorldLoad);
		eventBus.addListener(ClientEventHandler::onEntityTick);
		eventBus.addListener(BackpackTemplateStorage::onClientWorldLoad);
	}

	private static void registerBlockStateModels(RegisterBlockStateModels event) {
		event.registerModel(BackpackBlockModel.UnbakedBlockStateModel.ID, BackpackBlockModel.UnbakedBlockStateModel.CODEC);
	}

	private static void registerBackpackItemModels(RegisterItemModelsEvent event) {
		event.register(SophisticatedBackpacks.getIdentifier("backpack"), BackpackItemModel.Unbaked.MAP_CODEC);
	}

	private static void onEntityTick(EntityTickEvent.Post event) {
		Entity entity = event.getEntity();
		if (entity instanceof Player player) {
			PlayerInventoryProvider.get().getBackpackFromRendered(player, false).ifPresent(backpackRenderData -> {
				ItemStack backpack = backpackRenderData.getBackpack();
				if (BackpackItem.shouldRenderUpgradeActivity(backpack)) {
					IBackpackWrapper wrapper = BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent()
							? new BackpackWrapper(backpack)
							: BackpackWrapper.fromStack(backpack);
					clientTickUpgrades(player, wrapper.getRenderDataHandler());
				}
			});
		} else if (entity instanceof LivingEntity livingEntity) {
			ItemStack chestStack = livingEntity.getItemBySlot(EquipmentSlot.CHEST);
			if (chestStack.getItem() instanceof BackpackItem) {
				if (BackpackItem.shouldRenderUpgradeActivity(chestStack)) {
					IBackpackWrapper wrapper = BackpackItem.getLinkedStorageEndpointRole(chestStack).isPresent()
							? new BackpackWrapper(chestStack)
							: BackpackWrapper.fromStack(chestStack);
					clientTickUpgrades(livingEntity, wrapper.getRenderDataHandler());
				}
			}
		}
	}

	private static void clientTickUpgrades(LivingEntity livingEntity, RenderDataHandler renderDataHandler) {
		if (Minecraft.getInstance().isPaused() || livingEntity.level().getRandom().nextInt(32) != 0) {
			return;
		}
		renderDataHandler.getUpgradeClientData().forEach((type, data) -> UpgradeClientRegistry.getUpgradeClientTickHandler(type)
				.ifPresent(renderer -> renderUpgrade(renderer, livingEntity, type, data)));
	}

	private static Vector3f getBackpackMiddleFacePoint(LivingEntity livingEntity, Vector3f vector) {
		Vector3f point = new Vector3f(vector);
		boolean isCrouching = livingEntity.isCrouching();
		point.rotate(Axis.XP.rotationDegrees(isCrouching ? 25 : 0));
		point.add(0, 0.8f, isCrouching ? 0.9f : 0.7f);
		point.rotate(Axis.YN.rotationDegrees(livingEntity.yBodyRot - 180));
		point.add(livingEntity.position().toVector3f());
		return point;
	}

	private static <T extends IUpgradeClientData> void renderUpgrade(IUpgradeClientTickHandler<T> renderer, LivingEntity livingEntity,
			UpgradeClientDataType<?> type, IUpgradeClientData data) {
		// noinspection unchecked
		type.cast(data).ifPresent(clientData -> renderer.onClientTick(livingEntity.level(), livingEntity.level().getRandom(),
				vector3d -> getBackpackMiddleFacePoint(livingEntity, vector3d), (T) clientData));
	}

	private static void registerBackpackEntityRenderStateModifier(RegisterRenderStateModifiersEvent event) {
		// noinspection unchecked
		event.registerEntityModifier((Class<EntityRenderer<LivingEntity, LivingEntityRenderState>>) (Class<?>) LivingEntityRenderer.class,
				BackpackLayerRenderer.RENDER_STATE_MODIFIER);
	}

	private static void onPlayerLoggingIn(ClientPlayerNetworkEvent.LoggingIn event) {
		MobCatcherCaptureEffectRenderer.clear();
		ClientPacketDistributor.sendToServer(new RequestPlayerSettingsPayload());
	}

	private static void onPlayerLoggingOut(ClientPlayerNetworkEvent.LoggingOut event) {
		MobCatcherCaptureEffectRenderer.clear();
		ClientLinkedStorageBackpackContents.clear();
	}

	private static void submitCustomGeometry(SubmitCustomGeometryEvent event) {
		float partialTick = Minecraft.getInstance().getDeltaTracker().getGameTimeDeltaPartialTick(false);
		MobCatcherCaptureEffectRenderer.submit(event.getSubmitNodeCollector(), event.getPoseStack(), partialTick,
				event.getLevelRenderState().cameraRenderState);
	}

	private static void onModelRegistry(ModelEvent.RegisterLoaders event) {
		event.register(Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, BACKPACK_REG_NAME), BackpackBlockModel.Loader.INSTANCE);
	}

	public static void registerReloadListener(AddClientReloadListenersEvent event) {
		event.addListener(SophisticatedBackpacks.getIdentifier("backpack_layer_registration"), (ResourceManagerReloadListener) resourceManager -> {
			registerBackpackLayer(resourceManager);
			BackpackShapes.reloadDefaultShapeProvider(resourceManager);
			ClientBackpackShapeProvider.INSTANCE.rebuildShapes();
			BackpackShapes.setShapeProvider(ClientBackpackShapeProvider.INSTANCE);
		});
	}

	private static void registerEntityRenderers(EntityRenderersEvent.RegisterRenderers event) {
		event.registerEntityRenderer(EVERLASTING_BACKPACK_ITEM_ENTITY.get(), ItemEntityRenderer::new);
		event.registerBlockEntityRenderer(ModBlocks.BACKPACK_TILE_TYPE.get(), BackpackBlockEntityRenderer::new);
	}

	@SuppressWarnings("java:S3740") // explanation below
	private static void registerBackpackLayer(ResourceManager resourceManager) {
		EntityRenderDispatcher renderManager = Minecraft.getInstance().getEntityRenderDispatcher();
		Map<PlayerModelType, EntityRenderer<? extends Avatar, ?>> playerRenderers = renderManager.getPlayerRenderers();
		for (EntityRenderer<? extends Avatar, ?> renderer : playerRenderers.values()) {
			if (renderer instanceof LivingEntityRenderer<?, ?, ?> livingEntityRenderer) {
				// noinspection rawtypes ,unchecked - this is not going to fail as the LivingRenderer makes sure the types are right, but there doesn't seem to
				// be a way to us inference here
				livingEntityRenderer.addLayer(new BackpackLayerRenderer(livingEntityRenderer));
			}
		}

		renderManager.renderers.forEach((e, r) -> {
			if (r instanceof LivingEntityRenderer<?, ?, ?> livingEntityRenderer) {
				// noinspection rawtypes ,unchecked - this is not going to fail as the LivingRenderer makes sure the types are right, but there doesn't seem to
				// be a way to us inference here
				livingEntityRenderer.addLayer(new BackpackLayerRenderer(livingEntityRenderer));
			}
		});
	}

	public static void handleBlockPick(InputEvent.InteractionKeyMappingTriggered event) {
		Minecraft mc = Minecraft.getInstance();
		LocalPlayer player = mc.player;
		if (player == null || player.isCreative() || !event.isPickBlock() || mc.hitResult == null || mc.hitResult.getType() != HitResult.Type.BLOCK) {
			return;
		}
		HitResult target = mc.hitResult;
		Level level = player.level();
		BlockPos pos = ((BlockHitResult) target).getBlockPos();
		BlockState state = level.getBlockState(pos);

		if (state.isAir()) {
			return;
		}

		ItemStack result = state.getCloneItemStack(pos, level, true, player);

		if (result.isEmpty() || player.getInventory().findSlotMatchingItem(result) > -1) {
			return;
		}

		ClientPacketDistributor.sendToServer(new BlockPickPayload(result));
	}
}
