package net.p3pp3rf1y.sophisticatedcore.client;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.gui.screens.inventory.CreativeModeInventoryScreen;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.client.event.ClientPlayerNetworkEvent;
import net.minecraftforge.client.event.ScreenEvent;
import net.minecraftforge.client.event.TextureStitchEvent;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.api.IStashStorageItem;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;
import net.p3pp3rf1y.sophisticatedcore.init.ModParticles;
import net.p3pp3rf1y.sophisticatedcore.network.StorageInsertMessage;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.StorageSoundHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.util.RecipeHelper;

import java.util.Collections;

public class ClientEventHandler {
	private ClientEventHandler() {}

	public static void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(ClientEventHandler::stitchTextures);
		modBus.addListener(ModParticles::registerFactories);
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(ClientEventHandler::onPlayerJoinServer);
		eventBus.addListener(StorageSoundHandler::tick);
		eventBus.addListener(StorageSoundHandler::onWorldUnload);
		eventBus.addListener(ClientEventHandler::onDrawScreen);
		eventBus.addListener(EventPriority.HIGH, ClientEventHandler::onRightClick);
	}

	public static void stitchTextures(TextureStitchEvent.Pre evt) {
		if (evt.getAtlas().location() == InventoryMenu.BLOCK_ATLAS) {
			evt.addSprite(StorageContainerMenuBase.EMPTY_UPGRADE_SLOT_BACKGROUND);
			evt.addSprite(TankUpgradeContainer.EMPTY_TANK_INPUT_SLOT_BACKGROUND);
			evt.addSprite(TankUpgradeContainer.EMPTY_TANK_OUTPUT_SLOT_BACKGROUND);
			evt.addSprite(BatteryUpgradeContainer.EMPTY_BATTERY_INPUT_SLOT_BACKGROUND);
			evt.addSprite(BatteryUpgradeContainer.EMPTY_BATTERY_OUTPUT_SLOT_BACKGROUND);
		}
	}

	private static void onDrawScreen(ScreenEvent.DrawScreenEvent.Post event) {
		Minecraft mc = Minecraft.getInstance();
		Screen gui = mc.screen;
		if (!(gui instanceof AbstractContainerScreen<?> containerGui) || gui instanceof CreativeModeInventoryScreen || mc.player == null) {
			return;
		}
		AbstractContainerMenu menu = containerGui.getMenu();
		ItemStack held = menu.getCarried();
		if (!held.isEmpty()) {
			Slot under = containerGui.getSlotUnderMouse();
			PoseStack poseStack = event.getPoseStack();

			for (Slot s : menu.slots) {
				ItemStack stack = s.getItem();
				if (!s.mayPickup(mc.player) || stack.getCount() != 1 || !(stack.getItem() instanceof IStashStorageItem stashStorageItem) || !stashStorageItem.isItemStashable(stack, held)) {
					continue;
				}

				if (s == under) {
					int x = event.getMouseX();
					int y = event.getMouseY();
					poseStack.pushPose();
					poseStack.translate(0, 0, containerGui instanceof StorageScreenBase ? -100 : 100);
					containerGui.renderTooltip(poseStack, Collections.singletonList(new TranslatableComponent(TranslationHelper.INSTANCE.translItemTooltip("storage") + ".right_click_to_add_to_storage")), stashStorageItem.getInventoryTooltip(stack), x, y, mc.font);
					poseStack.popPose();
				} else {
					int x = containerGui.getGuiLeft() + s.x;
					int y = containerGui.getGuiTop() + s.y;

					poseStack.pushPose();
					poseStack.translate(0, 0, containerGui instanceof StorageScreenBase ? 100 : 499);

					mc.font.drawShadow(poseStack, "+", (float) x + 10, (float) y + 8, 0xFFFF00);
					poseStack.popPose();
				}
			}
		}
	}

	private static void onRightClick(ScreenEvent.MouseReleasedEvent.Pre event) {
		Minecraft mc = Minecraft.getInstance();
		Screen screen = mc.screen;
		if (screen instanceof AbstractContainerScreen<?> container && !(screen instanceof CreativeModeInventoryScreen) && event.getButton() == 1) {
			Slot under = container.getSlotUnderMouse();
			ItemStack held = container.getMenu().getCarried();

			if (under != null && !held.isEmpty() && mc.player != null && under.mayPickup(mc.player)) {
				ItemStack stack = under.getItem();
				if (stack.getItem() instanceof IStashStorageItem && stack.getCount() == 1) {
					SophisticatedCore.PACKET_HANDLER.sendToServer(new StorageInsertMessage(under.index));
					screen.mouseReleased(0, 0, -1);
					event.setCanceled(true);
				}
			}
		}
	}

	private static void onPlayerJoinServer(ClientPlayerNetworkEvent.LoggedInEvent evt) {
		//noinspection ConstantConditions - by the time player is joining the world is not null
		RecipeHelper.setWorld(Minecraft.getInstance().level);
	}
}
