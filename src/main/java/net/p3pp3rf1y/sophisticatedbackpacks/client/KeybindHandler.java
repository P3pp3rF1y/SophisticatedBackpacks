package net.p3pp3rf1y.sophisticatedbackpacks.client;

import com.mojang.blaze3d.platform.InputConstants;
import net.minecraft.client.KeyMapping;
import net.minecraft.client.Minecraft;
import net.minecraft.client.MouseHandler;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.phys.BlockHitResult;
import net.minecraft.world.phys.EntityHitResult;
import net.minecraft.world.phys.HitResult;
import net.minecraftforge.client.ClientRegistry;
import net.minecraftforge.client.event.ScreenEvent;
import net.minecraftforge.client.settings.IKeyConflictContext;
import net.minecraftforge.client.settings.KeyConflictContext;
import net.minecraftforge.client.settings.KeyModifier;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.eventbus.api.EventPriority;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent;
import net.minecraftforge.items.CapabilityItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BlockToolSwapMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.EntityToolSwapMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.InventoryInteractionMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.UpgradeToggleMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import java.util.Map;
import java.util.Optional;

import static net.minecraftforge.client.settings.KeyConflictContext.GUI;

public class KeybindHandler {
	private KeybindHandler() {}

	private static final int KEY_B = 66;
	private static final int KEY_C = 67;
	private static final int KEY_Z = 90;
	private static final int KEY_X = 88;
	private static final int KEY_UNKNOWN = -1;
	private static final int MIDDLE_BUTTON = 2;
	private static final int CHEST_SLOT_INDEX = 38;
	private static final int OFFHAND_SLOT_INDEX = 40;
	private static final String KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY = "keybind.sophisticatedbackpacks.category";
	public static final KeyMapping BACKPACK_TOGGLE_UPGRADE_5 = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("toggle_upgrade_5"),
			KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping BACKPACK_TOGGLE_UPGRADE_4 = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("toggle_upgrade_4"),
			KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping BACKPACK_TOGGLE_UPGRADE_3 = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("toggle_upgrade_3"),
			KeyConflictContext.UNIVERSAL, InputConstants.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping BACKPACK_TOGGLE_UPGRADE_2 = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("toggle_upgrade_2"),
			KeyConflictContext.UNIVERSAL, KeyModifier.ALT, InputConstants.Type.KEYSYM.getOrCreate(KEY_X), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping BACKPACK_TOGGLE_UPGRADE_1 = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("toggle_upgrade_1"),
			KeyConflictContext.UNIVERSAL, KeyModifier.ALT, InputConstants.Type.KEYSYM.getOrCreate(KEY_Z), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);

	public static final Map<Integer, KeyMapping> UPGRADE_SLOT_TOGGLE_KEYBINDS = Map.of(
			0, BACKPACK_TOGGLE_UPGRADE_1,
			1, BACKPACK_TOGGLE_UPGRADE_2,
			2, BACKPACK_TOGGLE_UPGRADE_3,
			3, BACKPACK_TOGGLE_UPGRADE_4,
			4, BACKPACK_TOGGLE_UPGRADE_5
	);
	public static final KeyMapping SORT_KEYBIND = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("sort"),
			BackpackGuiKeyConflictContext.INSTANCE, InputConstants.Type.MOUSE.getOrCreate(MIDDLE_BUTTON), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping TOOL_SWAP_KEYBIND = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("tool_swap"),
			KeyConflictContext.IN_GAME, InputConstants.Type.KEYSYM.getOrCreate(KEY_UNKNOWN), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping INVENTORY_INTERACTION_KEYBIND = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("inventory_interaction"),
			KeyConflictContext.IN_GAME, InputConstants.Type.KEYSYM.getOrCreate(KEY_C), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);
	public static final KeyMapping BACKPACK_OPEN_KEYBIND = new KeyMapping(SBPTranslationHelper.INSTANCE.translKeybind("open_backpack"),
			BackpackKeyConflictContext.INSTANCE, InputConstants.Type.KEYSYM.getOrCreate(KEY_B), KEYBIND_SOPHISTICATEDBACKPACKS_CATEGORY);

	public static void register(FMLClientSetupEvent event) {
		IEventBus eventBus = MinecraftForge.EVENT_BUS;
		eventBus.addListener(EventPriority.HIGH, KeybindHandler::handleKeyInputEvent);
		eventBus.addListener(EventPriority.HIGH, KeybindHandler::handleGuiMouseKeyPress);
		eventBus.addListener(EventPriority.HIGH, KeybindHandler::handleGuiKeyPress);

		event.enqueueWork(() -> {
			ClientRegistry.registerKeyBinding(BACKPACK_OPEN_KEYBIND);
			ClientRegistry.registerKeyBinding(INVENTORY_INTERACTION_KEYBIND);
			ClientRegistry.registerKeyBinding(TOOL_SWAP_KEYBIND);
			ClientRegistry.registerKeyBinding(SORT_KEYBIND);
			UPGRADE_SLOT_TOGGLE_KEYBINDS.forEach((slot, keybind) -> ClientRegistry.registerKeyBinding(keybind));
		});
	}

	public static void handleGuiKeyPress(ScreenEvent.KeyboardKeyPressedEvent.Pre event) {
		InputConstants.Key key = InputConstants.getKey(event.getKeyCode(), event.getScanCode());
		if (SORT_KEYBIND.isActiveAndMatches(key) && tryCallSort(event.getScreen()) || BACKPACK_OPEN_KEYBIND.isActiveAndMatches(key) && sendBackpackOpenOrCloseMessage()) {
			event.setCanceled(true);
		}
	}

	public static void handleGuiMouseKeyPress(ScreenEvent.MouseClickedEvent.Pre event) {
		InputConstants.Key input = InputConstants.Type.MOUSE.getOrCreate(event.getButton());
		if (SORT_KEYBIND.isActiveAndMatches(input) && tryCallSort(event.getScreen()) || BACKPACK_OPEN_KEYBIND.isActiveAndMatches(input) && sendBackpackOpenOrCloseMessage()) {
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
			for (Map.Entry<Integer, KeyMapping> slotKeybind : UPGRADE_SLOT_TOGGLE_KEYBINDS.entrySet()) {
				if (slotKeybind.getValue().consumeClick()) {
					SBPPacketHandler.INSTANCE.sendToServer(new UpgradeToggleMessage(slotKeybind.getKey()));
				}
			}
		}
	}

	private static boolean tryCallSort(Screen gui) {
		Minecraft mc = Minecraft.getInstance();
		if (mc.player != null && mc.player.containerMenu instanceof BackpackContainer container && gui instanceof BackpackScreen screen) {
			MouseHandler mh = mc.mouseHandler;
			double mouseX = mh.xpos() * mc.getWindow().getGuiScaledWidth() / mc.getWindow().getScreenWidth();
			double mouseY = mh.ypos() * mc.getWindow().getGuiScaledHeight() / mc.getWindow().getScreenHeight();
			Slot selectedSlot = screen.findSlot(mouseX, mouseY);
			if (selectedSlot != null && container.isNotPlayersInventorySlot(selectedSlot.index)) {
				container.sort();
				return true;
			}
		}
		return false;
	}

	private static void sendToolSwapMessage() {
		Minecraft mc = Minecraft.getInstance();
		LocalPlayer player = mc.player;
		if (player == null || mc.hitResult == null) {
			return;
		}
		if (player.getMainHandItem().getItem() instanceof BackpackItem) {
			player.displayClientMessage(new TranslatableComponent("gui.sophisticatedbackpacks.status.unable_to_swap_tool_for_backpack"), true);
			return;
		}
		HitResult rayTrace = mc.hitResult;
		if (rayTrace.getType() == HitResult.Type.BLOCK) {
			BlockHitResult blockRayTraceResult = (BlockHitResult) rayTrace;
			BlockPos pos = blockRayTraceResult.getBlockPos();
			SBPPacketHandler.INSTANCE.sendToServer(new BlockToolSwapMessage(pos));
		} else if (rayTrace.getType() == HitResult.Type.ENTITY) {
			EntityHitResult entityRayTraceResult = (EntityHitResult) rayTrace;
			SBPPacketHandler.INSTANCE.sendToServer(new EntityToolSwapMessage(entityRayTraceResult.getEntity().getId()));
		}
	}

	private static void sendInteractWithInventoryMessage() {
		Minecraft mc = Minecraft.getInstance();
		HitResult rayTrace = mc.hitResult;
		if (rayTrace == null || rayTrace.getType() != HitResult.Type.BLOCK) {
			return;
		}
		BlockHitResult blockraytraceresult = (BlockHitResult) rayTrace;
		BlockPos pos = blockraytraceresult.getBlockPos();

		if (!WorldHelper.getBlockEntity(mc.level, pos, BlockEntity.class).map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).isPresent()).orElse(false)) {
			return;
		}

		SBPPacketHandler.INSTANCE.sendToServer(new InventoryInteractionMessage(pos, blockraytraceresult.getDirection()));
	}

	@SuppressWarnings({"java:S2440", "InstantiationOfUtilityClass"})
	private static boolean sendBackpackOpenOrCloseMessage() {
		if (!GUI.isActive()) {
			SBPPacketHandler.INSTANCE.sendToServer(new BackpackOpenMessage());
			return false;
		}

		Screen screen = Minecraft.getInstance().screen;
		if (screen instanceof AbstractContainerScreen<?> containerScreen) {
			Slot slot = containerScreen.getSlotUnderMouse();

			if (slot != null && slot.container instanceof Inventory) {
				Optional<String> handlerName = getPlayerInventoryHandlerName(slot.getSlotIndex());

				if (handlerName.isPresent() && slot.getItem().getItem() instanceof BackpackItem) {
					SBPPacketHandler.INSTANCE.sendToServer(new BackpackOpenMessage(slot.getSlotIndex(), "", handlerName.get()));
					return true;
				}
			}
			if (screen instanceof BackpackScreen && slot != null && slot.getItem().getItem() instanceof BackpackItem && slot.getItem().getCount() == 1) {
				SBPPacketHandler.INSTANCE.sendToServer(new BackpackOpenMessage(slot.index));
				return true;
			}
		}
		return false;
	}

	private static Optional<String> getPlayerInventoryHandlerName(int slotIndex) {
		if (slotIndex == CHEST_SLOT_INDEX) {
			return Optional.of(PlayerInventoryProvider.ARMOR_INVENTORY);
		} else  if (slotIndex == OFFHAND_SLOT_INDEX) {
			return Optional.of(PlayerInventoryProvider.OFFHAND_INVENTORY);
		} else if (slotIndex >= 0 && slotIndex < 36) {
			return Optional.of(PlayerInventoryProvider.MAIN_INVENTORY);
		}

		return Optional.empty();
	}

	private static class BackpackKeyConflictContext implements IKeyConflictContext {
		public static final BackpackKeyConflictContext INSTANCE = new BackpackKeyConflictContext();

		@Override
		public boolean isActive() {
			return !GUI.isActive() || Minecraft.getInstance().screen instanceof AbstractContainerScreen<?>;
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
