//package net.p3pp3rf1y.sophisticatedcore.compat.craftingtweaks;
//
//import net.blay09.mods.craftingtweaks.CraftingTweaksProviderManager;
//import net.blay09.mods.craftingtweaks.api.CraftingTweaksClientAPI;
//import net.minecraft.client.gui.components.AbstractWidget;
//import net.minecraft.client.gui.components.Button;
//import net.minecraft.client.gui.components.events.GuiEventListener;
//import net.minecraft.client.gui.screens.Screen;
//import net.minecraft.world.inventory.Slot;
//import net.minecraftforge.api.distmarker.Dist;
//import net.minecraftforge.api.distmarker.OnlyIn;
//import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
//import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
//import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
//import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.ICraftingUIPart;
//
//import java.lang.reflect.InvocationTargetException;
//import java.lang.reflect.Method;
//import java.util.ArrayList;
//import java.util.List;
//
//public class CraftingUpgradeTweakUIPart implements ICraftingUIPart {
//	@OnlyIn(Dist.CLIENT)
//	private StorageScreenBase<?> storageScreen;
//
//	private static final Method ADD_RENDERABLE_WIDGET = ObfuscationReflectionHelper.findMethod(Screen.class, "m_142416_", GuiEventListener.class);
//
//	private final List<Button> buttons = new ArrayList<>();
//
//	public static void register() {
//		StorageScreenBase.setCraftingUIPart(new CraftingUpgradeTweakUIPart());
//	}
//
//	@OnlyIn(Dist.CLIENT)
//	private void addButton(Button button) {
//		buttons.add(button);
//		try {
//			ADD_RENDERABLE_WIDGET.invoke(storageScreen, button);
//		}
//		catch (IllegalAccessException | InvocationTargetException e) {
//			SophisticatedCore.LOGGER.error("Error calling addButton in Screen class", e);
//		}
//	}
//
//	@Override
//	@OnlyIn(Dist.CLIENT)
//	public void onCraftingSlotsHidden() {
//		if (buttons.isEmpty()) {
//			return;
//		}
//
//		List<GuiEventListener> screenChildren = ObfuscationReflectionHelper.getPrivateValue(Screen.class, storageScreen, "f_96540_");
//		List<AbstractWidget> screenRenderables = ObfuscationReflectionHelper.getPrivateValue(Screen.class, storageScreen, "f_169369_");
//		if (screenChildren == null || screenRenderables == null) {
//			return;
//		}
//
//		buttons.forEach(screenChildren::remove);
//		buttons.forEach(screenRenderables::remove);
//		buttons.clear();
//	}
//
//	@Override
//	public int getWidth() {
//		return 18;
//	}
//
//	@Override
//	@OnlyIn(Dist.CLIENT)
//	public void setStorageScreen(StorageScreenBase<?> screen) {
//		storageScreen = screen;
//	}
//
//	@Override
//	public void onCraftingSlotsDisplayed(List<Slot> slots) {
//		if (slots.isEmpty()) {
//			return;
//		}
//		Slot firstSlot = slots.get(0);
//		CraftingTweaksProviderManager.getDefaultCraftingGrid(storageScreen.getMenu()).ifPresent(craftingGrid -> {
//			addButton(CraftingTweaksClientAPI.createRotateButtonRelative(craftingGrid, storageScreen, getButtonX(firstSlot), getButtonY(firstSlot, 0)));
//			addButton(CraftingTweaksClientAPI.createBalanceButtonRelative(craftingGrid, storageScreen, getButtonX(firstSlot), getButtonY(firstSlot, 1)));
//			addButton(CraftingTweaksClientAPI.createClearButtonRelative(craftingGrid, storageScreen, getButtonX(firstSlot), getButtonY(firstSlot, 2)));
//		});
//	}
//
//	@OnlyIn(Dist.CLIENT)
//	private int getButtonX(Slot firstSlot) {
//		return firstSlot.x - 19;
//	}
//
//	@OnlyIn(Dist.CLIENT)
//	private int getButtonY(Slot firstSlot, int index) {
//		return firstSlot.y + 18 * index;
//	}
//}
