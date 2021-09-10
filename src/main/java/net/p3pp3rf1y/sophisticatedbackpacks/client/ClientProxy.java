package net.p3pp3rf1y.sophisticatedbackpacks.client;

import com.google.common.collect.ImmutableMap;
import net.minecraft.client.Minecraft;
import net.minecraft.client.MouseHelper;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.client.gui.screen.Screen;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.RenderTypeLookup;
import net.minecraft.client.renderer.entity.EntityRendererManager;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.LivingRenderer;
import net.minecraft.client.renderer.entity.PlayerRenderer;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.client.util.InputMappings;
import net.minecraft.inventory.container.PlayerContainer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.math.BlockRayTraceResult;
import net.minecraft.util.math.EntityRayTraceResult;
import net.minecraft.util.math.RayTraceResult;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.client.event.ClientPlayerNetworkEvent;
import net.minecraftforge.client.event.GuiScreenEvent;
import net.minecraftforge.client.event.ModelRegistryEvent;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.client.model.ModelLoaderRegistry;
import net.minecraftforge.client.settings.IKeyConflictContext;
import net.minecraftforge.client.settings.KeyConflictContext;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.client.registry.RenderingRegistry;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModBlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModItemColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackDynamicModel;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackTESR;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackTooltipRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonProxy;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackCloseMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BlockToolSwapMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.EntityToolSwapMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.InventoryInteractionMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.UpgradeToggleMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.BackpackSoundHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperFilterContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import java.util.Map;

import static net.minecraftforge.client.settings.KeyConflictContext.GUI;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translKeybind;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY;

public class ClientProxy extends CommonProxy {
	private static final int KEY_B = 66;
	private static final int KEY_C = 67;
	private static final int KEY_Z = 90;
	private static final int KEY_X = 88;
	private static final int KEY_UNKNOWN = -1;
	private static final int MIDDLE_BUTTON = 2;

	private static final String KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY = "keybind.sophisticatedbackpacks.category";

	public static final KeyBinding BACKPACK_OPEN_KEYBIND = new KeyBinding(translKeybind("open_backpack"),
			BackpackKeyConflictContext.INSTANCE, InputMappings.Type.KEYSYM.getOrCreate(KEY_B), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding INVENTORY_INTERACTION_KEYBIND = new KeyBinding(translKeybind("inventory_interaction"),
			KeyConflictContext.IN_GAME, InputMappings.Type.KEYSYM.getOrCreate(KEY_C), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding TOOL_SWAP_KEYBIND = new KeyBinding(translKeybind("tool_swap"),
			KeyConflictContext.IN_GAME, InputMappings.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding SORT_KEYBIND = new KeyBinding(translKeybind("sort"),
			BackpackGuiKeyConflictContext.INSTANCE, InputMappings.Type.MOUSE.getOrCreate(MIDDLE_BUTTON), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);

	public static final KeyBinding BACKPACK_TOGGLE_UPGRADE_1 = new KeyBinding(translKeybind("toggle_upgrade_1"),
			KeyConflictContext.UNIVERSAL, KeyModifier.ALT, InputMappings.Type.KEYSYM.getOrCreate(KEY_Z), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding BACKPACK_TOGGLE_UPGRADE_2 = new KeyBinding(translKeybind("toggle_upgrade_2"),
			KeyConflictContext.UNIVERSAL, KeyModifier.ALT, InputMappings.Type.KEYSYM.getOrCreate(KEY_X), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding BACKPACK_TOGGLE_UPGRADE_3 = new KeyBinding(translKeybind("toggle_upgrade_3"),
			KeyConflictContext.UNIVERSAL, InputMappings.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding BACKPACK_TOGGLE_UPGRADE_4 = new KeyBinding(translKeybind("toggle_upgrade_4"),
			KeyConflictContext.UNIVERSAL, InputMappings.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyBinding BACKPACK_TOGGLE_UPGRADE_5 = new KeyBinding(translKeybind("toggle_upgrade_5"),
			KeyConflictContext.UNIVERSAL, InputMappings.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);

	private static final Map<Integer, KeyBinding> UPGRADE_SLOT_TOGGLE_KEYBINDS = ImmutableMap.of(
			0, BACKPACK_TOGGLE_UPGRADE_1,
			1, BACKPACK_TOGGLE_UPGRADE_2,
			2, BACKPACK_TOGGLE_UPGRADE_3,
			3, BACKPACK_TOGGLE_UPGRADE_4,
			4, BACKPACK_TOGGLE_UPGRADE_5
	);

	private static boolean tryCallSort(Screen gui) {
		Minecraft mc = Minecraft.getInstance();
		if (mc.player != null && mc.player.containerMenu instanceof BackpackContainer && gui instanceof BackpackScreen) {
			BackpackScreen screen = (BackpackScreen) gui;
			MouseHelper mh = mc.mouseHandler;
			double mouseX = mh.xpos() * mc.getWindow().getGuiScaledWidth() / mc.getWindow().getScreenWidth();
			double mouseY = mh.ypos() * mc.getWindow().getGuiScaledHeight() / mc.getWindow().getScreenHeight();
			BackpackContainer container = (BackpackContainer) mc.player.containerMenu;
			Slot selectedSlot = screen.findSlot(mouseX, mouseY);
			if (selectedSlot != null && !container.isPlayersInventorySlot(selectedSlot.index)) {
				container.sort();
				return true;
			}
		}
		return false;
	}

	public static void handleGuiKeyPress(GuiScreenEvent.KeyboardKeyPressedEvent.Pre event) {
		if (SORT_KEYBIND.isActiveAndMatches(InputMappings.getKey(event.getKeyCode(), event.getScanCode())) && tryCallSort(event.getGui())) {
			event.setCanceled(true);
		}
	}

	public static void handleGuiMouseKeyPress(GuiScreenEvent.MouseClickedEvent.Pre event) {
		if (SORT_KEYBIND.isActiveAndMatches(InputMappings.Type.MOUSE.getOrCreate(event.getButton())) && tryCallSort(event.getGui())) {
			event.setCanceled(true);
		}
	}

	public static void handleKeyInputEvent(TickEvent.ClientTickEvent event) {
		if (BACKPACK_OPEN_KEYBIND.consumeClick()) {
			sendBackpackOpenOrCloseMessage();
		} else if (INVENTORY_INTERACTION_KEYBIND.consumeClick()) {
			sendInteractWithInventoryMessage();
		} else if (TOOL_SWAP_KEYBIND.consumeClick()) {
			sendToolSwapMessage();
		} else {
			for (Map.Entry<Integer, KeyBinding> slotKeybind : UPGRADE_SLOT_TOGGLE_KEYBINDS.entrySet()) {
				if (slotKeybind.getValue().consumeClick()) {
					PacketHandler.sendToServer(new UpgradeToggleMessage(slotKeybind.getKey()));
				}
			}
		}
	}

	private static void sendToolSwapMessage() {
		Minecraft mc = Minecraft.getInstance();
		ClientPlayerEntity player = mc.player;
		if (player == null || mc.hitResult == null) {
			return;
		}
		if (player.getMainHandItem().getItem() instanceof BackpackItem) {
			player.displayClientMessage(new TranslationTextComponent("gui.sophisticatedbackpacks.status.unable_to_swap_tool_for_backpack"), true);
			return;
		}
		RayTraceResult rayTrace = mc.hitResult;
		if (rayTrace.getType() == RayTraceResult.Type.BLOCK) {
			BlockRayTraceResult blockRayTraceResult = (BlockRayTraceResult) rayTrace;
			BlockPos pos = blockRayTraceResult.getBlockPos();
			PacketHandler.sendToServer(new BlockToolSwapMessage(pos));
		} else if (rayTrace.getType() == RayTraceResult.Type.ENTITY) {
			EntityRayTraceResult entityRayTraceResult = (EntityRayTraceResult) rayTrace;
			PacketHandler.sendToServer(new EntityToolSwapMessage(entityRayTraceResult.getEntity().getId()));
		}
	}

	private static void sendInteractWithInventoryMessage() {
		Minecraft mc = Minecraft.getInstance();
		RayTraceResult rayTrace = mc.hitResult;
		if (rayTrace == null || rayTrace.getType() != RayTraceResult.Type.BLOCK) {
			return;
		}
		BlockRayTraceResult blockraytraceresult = (BlockRayTraceResult) rayTrace;
		BlockPos pos = blockraytraceresult.getBlockPos();

		if (!WorldHelper.getTile(mc.level, pos, TileEntity.class).map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).isPresent()).orElse(false)) {
			return;
		}

		PacketHandler.sendToServer(new InventoryInteractionMessage(pos, blockraytraceresult.getDirection()));
	}

	@SuppressWarnings({"java:S2440", "InstantiationOfUtilityClass"})
	private static void sendBackpackOpenOrCloseMessage() {
		if (!GUI.isActive()) {
			PacketHandler.sendToServer(new BackpackOpenMessage());
		} else if (Minecraft.getInstance().screen instanceof BackpackScreen) {
			BackpackScreen backpackScreen = (BackpackScreen) Minecraft.getInstance().screen;

			Slot slot = backpackScreen.getSlotUnderMouse();
			if (slot != null && slot.getItem().getItem() instanceof BackpackItem) {
				PacketHandler.sendToServer(new BackpackOpenMessage(slot.index));
			} else {
				PacketHandler.sendToServer(new BackpackCloseMessage());
			}
		}
	}

	@Override
	public void registerHandlers() {
		super.registerHandlers();
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(this::loadComplete);
		modBus.addListener(this::clientSetup);
		modBus.addListener(this::stitchTextures);
		modBus.addListener(this::onModelRegistry);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(ClientProxy::handleKeyInputEvent);
		eventBus.addListener(EventPriority.HIGH, ClientProxy::handleGuiMouseKeyPress);
		eventBus.addListener(EventPriority.HIGH, ClientProxy::handleGuiKeyPress);
		eventBus.addListener(ClientProxy::onPlayerJoinServer);
		eventBus.addListener(BackpackTooltipRenderer::renderBackpackTooltip);
		eventBus.addListener(BackpackTooltipRenderer::onWorldLoad);
		eventBus.addListener(BackpackSoundHandler::tick);
		eventBus.addListener(BackpackSoundHandler::onWorldUnload);
	}

	private void loadComplete(FMLLoadCompleteEvent event) {
		event.enqueueWork(() -> {
			ModItemColors.init();
			ModBlockColors.init();
			registerBackpackLayer();
		});
	}

	private void onModelRegistry(ModelRegistryEvent event) {
		ModelLoaderRegistry.registerLoader(RegistryHelper.getRL("backpack"), BackpackDynamicModel.Loader.INSTANCE);
	}

	private void clientSetup(FMLClientSetupEvent event) {
		event.enqueueWork(() -> {
			ClientRegistry.registerKeyBinding(BACKPACK_OPEN_KEYBIND);
			ClientRegistry.registerKeyBinding(INVENTORY_INTERACTION_KEYBIND);
			ClientRegistry.registerKeyBinding(TOOL_SWAP_KEYBIND);
			ClientRegistry.registerKeyBinding(SORT_KEYBIND);
			UPGRADE_SLOT_TOGGLE_KEYBINDS.forEach((slot, keybind) -> ClientRegistry.registerKeyBinding(keybind));
		});
		RenderTypeLookup.setRenderLayer(ModBlocks.BACKPACK.get(), RenderType.cutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.IRON_BACKPACK.get(), RenderType.cutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.GOLD_BACKPACK.get(), RenderType.cutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.DIAMOND_BACKPACK.get(), RenderType.cutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.NETHERITE_BACKPACK.get(), RenderType.cutout());
		RenderingRegistry.registerEntityRenderingHandler(EVERLASTING_BACKPACK_ITEM_ENTITY.get(), renderManager -> new ItemRenderer(renderManager, Minecraft.getInstance().getItemRenderer()));
		ClientRegistry.bindTileEntityRenderer(ModBlocks.BACKPACK_TILE_TYPE.get(), BackpackTESR::new);
	}

	@SuppressWarnings("java:S3740") //explanation below
	private void registerBackpackLayer() {
		EntityRendererManager renderManager = Minecraft.getInstance().getEntityRenderDispatcher();
		Map<String, PlayerRenderer> skinMap = renderManager.getSkinMap();
		PlayerRenderer render = skinMap.get("default");
		render.addLayer(new BackpackLayerRenderer<>(render));
		render = skinMap.get("slim");
		render.addLayer(new BackpackLayerRenderer<>(render));
		renderManager.renderers.forEach((e, r) -> {
			if (r instanceof LivingRenderer<?, ?>) {
				//noinspection rawtypes ,unchecked - this is not going to fail as the LivingRenderer makes sure the types are right, but there doesn't seem to be a way to us inference here
				((LivingRenderer<?, ?>) r).addLayer(new BackpackLayerRenderer((LivingRenderer<?, ?>) r));
			}
		});
	}

	public void stitchTextures(TextureStitchEvent.Pre evt) {
		if (evt.getMap().location() == PlayerContainer.BLOCK_ATLAS) {
			evt.addSprite(BackpackContainer.EMPTY_UPGRADE_SLOT_BACKGROUND);
			evt.addSprite(ToolSwapperFilterContainer.EMPTY_WEAPON_SLOT_BACKGROUND);
			ToolSwapperFilterContainer.EMPTY_TOOL_SLOT_BACKGROUNDS.values().forEach(evt::addSprite);
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

	private static class BackpackKeyConflictContext implements IKeyConflictContext {
		public static final BackpackKeyConflictContext INSTANCE = new BackpackKeyConflictContext();

		@Override
		public boolean isActive() {
			return !GUI.isActive() || Minecraft.getInstance().screen instanceof BackpackScreen;
		}

		@Override
		public boolean conflicts(IKeyConflictContext other) {
			return this == other;
		}

	}

	private static class BackpackGuiKeyConflictContext implements IKeyConflictContext {
		public static final BackpackGuiKeyConflictContext INSTANCE = new BackpackGuiKeyConflictContext();

		@Override
		public boolean isActive() {
			return GUI.isActive() && Minecraft.getInstance().screen instanceof BackpackScreen;
		}

		@Override
		public boolean conflicts(IKeyConflictContext other) {
			return this == other;
		}
	}
}
