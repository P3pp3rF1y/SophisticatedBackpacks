package net.p3pp3rf1y.sophisticatedbackpacks.client;

import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.RenderTypeLookup;
import net.minecraft.client.settings.KeyBinding;
import net.minecraft.client.util.InputMappings;
import net.minecraftforge.client.settings.KeyConflictContext;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.client.registry.ClientRegistry;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.fml.event.lifecycle.FMLLoadCompleteEvent;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModBlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.ModItemColors;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonProxy;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

public class ClientProxy extends CommonProxy {
	private static final int KEY_B = 48;
	public static final KeyBinding BACKPACK_OPEN_KEYBIND = new KeyBinding("keybind.sophisticatedbackpacks.backpack.open",
			KeyConflictContext.IN_GAME, InputMappings.Type.KEYSYM.getOrMakeInput(KEY_B), "keybind.sophisticatedbackpacks.category");

	public static void handleKeyInputEvent(TickEvent.ClientTickEvent event) {
		if (BACKPACK_OPEN_KEYBIND.isPressed()) {
			PacketHandler.sendToServer(BackpackOpenMessage.getInstance());
		}
	}

	@Override
	public void registerHandlers() {
		super.registerHandlers();
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(this::loadComplete);
		modBus.addListener(this::clientSetup);
	}

	private void loadComplete(FMLLoadCompleteEvent event) {
		ModItemColors.init();
		ModBlockColors.init();
		MinecraftForge.EVENT_BUS.addListener(ClientProxy::handleKeyInputEvent);
	}

	private void clientSetup(FMLClientSetupEvent event) {
		event.enqueueWork(() -> ClientRegistry.registerKeyBinding(BACKPACK_OPEN_KEYBIND));
		RenderTypeLookup.setRenderLayer(ModBlocks.BACKPACK, RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.IRON_BACKPACK, RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.GOLD_BACKPACK, RenderType.getCutout());
		RenderTypeLookup.setRenderLayer(ModBlocks.DIAMOND_BACKPACK, RenderType.getCutout());
	}
}
