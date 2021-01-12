package net.p3pp3rf1y.sophisticatedbackpacks.client;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.RenderTypeLookup;
import net.minecraft.client.renderer.entity.ItemRenderer;
import net.minecraft.client.renderer.entity.PlayerRenderer;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.client.util.InputMappings;
import net.minecraft.inventory.container.Slot;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.ClientPlayerNetworkEvent;
import net.minecraftforge.client.settings.IKeyConflictContext;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.client.registry.RenderingRegistry;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModBlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModItemColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackLayerRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonProxy;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import java.util.Map;

import static net.minecraftforge.client.settings.KeyConflictContext.GUI;
import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.EVERLASTING_BACKPACK_ITEM_ENTITY;

@OnlyIn(Dist.CLIENT)
public class ClientProxy extends CommonProxy {
	private static final int KEY_B = 66;
	public static final KeyBinding BACKPACK_OPEN_KEYBIND = new KeyBinding("keybind.sophisticatedbackpacks.backpack.open",
			BackpackKeyConflictContext.INSTANCE, InputMappings.Type.KEYSYM.getOrMakeInput(KEY_B), "keybind.sophisticatedbackpacks.category");

	public static void handleKeyInputEvent(TickEvent.ClientTickEvent event) {
		if (BACKPACK_OPEN_KEYBIND.isPressed()) {
			if (!GUI.isActive()) {
				PacketHandler.sendToServer(new BackpackOpenMessage());
			} else {
				BackpackScreen backpackScreen = (BackpackScreen) Minecraft.getInstance().currentScreen;

				if (backpackScreen != null) {
					Slot slot = backpackScreen.getSlotUnderMouse();
					if (slot != null && slot.getStack().getItem() instanceof BackpackItem) {
						PacketHandler.sendToServer(new BackpackOpenMessage(slot.slotNumber));
					}
				}
			}
		}
	}

	@Override
	public void registerHandlers() {
		super.registerHandlers();
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(this::loadComplete);
		modBus.addListener(this::clientSetup);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(ClientProxy::handleKeyInputEvent);
		eventBus.addListener(ClientProxy::onPlayerJoinServer);
	}

	private void loadComplete(FMLLoadCompleteEvent event) {
		event.enqueueWork(() -> {
			ModItemColors.init();
			ModBlockColors.init();
			registerBackpackLayer();
		});
	}

	private void clientSetup(FMLClientSetupEvent event) {
		event.enqueueWork(() -> ClientRegistry.registerKeyBinding(BACKPACK_OPEN_KEYBIND));
		RenderTypeLookup.setRenderLayer(ModBlocks.BACKPACK.get(), RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.IRON_BACKPACK.get(), RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.GOLD_BACKPACK.get(), RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.DIAMOND_BACKPACK.get(), RenderType.getCutout());
		RenderingRegistry.registerEntityRenderingHandler(EVERLASTING_BACKPACK_ITEM_ENTITY.get(), renderManager -> new ItemRenderer(renderManager, Minecraft.getInstance().getItemRenderer()));
	}

	private void registerBackpackLayer() {
		Map<String, PlayerRenderer> skinMap = Minecraft.getInstance().getRenderManager().getSkinMap();
		PlayerRenderer render = skinMap.get("default");
		render.addLayer(new BackpackLayerRenderer(render));
		render = skinMap.get("slim");
		render.addLayer(new BackpackLayerRenderer(render));
	}

	private static void onPlayerJoinServer(ClientPlayerNetworkEvent.LoggedInEvent evt) {
		//noinspection ConstantConditions - by the time player is joining the world is not null
		RecipeHelper.setWorld(Minecraft.getInstance().world);
	}

	private static class BackpackKeyConflictContext implements IKeyConflictContext {
		public static final BackpackKeyConflictContext INSTANCE = new BackpackKeyConflictContext();

		@Override
		public boolean isActive() {
			return !GUI.isActive() || Minecraft.getInstance().currentScreen instanceof BackpackScreen;
		}

		@Override
		public boolean conflicts(IKeyConflictContext other) {
			return this == other;
		}
	}

}
