package net.p3pp3rf1y.sophisticatedbackpacks.compat.craftingtweaks;

import net.minecraft.client.gui.components.AbstractWidget;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.world.inventory.Slot;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.ICraftingUIPart;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

public class CraftingUpgradeTweakUIPart implements ICraftingUIPart {
	@OnlyIn(Dist.CLIENT)
	private BackpackScreen backpackScreen;

	private static final Method ADD_RENDERABLE_WIDGET = ObfuscationReflectionHelper.findMethod(Screen.class, "m_142416_", AbstractWidget.class);

	private final List<Button> buttons = new ArrayList<>();

	public static void register() {
		BackpackScreen.setCraftingUIPart(new CraftingUpgradeTweakUIPart());
	}

	@OnlyIn(Dist.CLIENT)
	private void addButton(Button button) {
		buttons.add(button);
		try {
			ADD_RENDERABLE_WIDGET.invoke(backpackScreen, button);
		}
		catch (IllegalAccessException | InvocationTargetException e) {
			SophisticatedBackpacks.LOGGER.error("Error calling addButton in Screen class", e);
		}
	}

	@Override
	@OnlyIn(Dist.CLIENT)
	public void onCraftingSlotsHidden() {
		if (buttons.isEmpty()) {
			return;
		}

		List<GuiEventListener> screenChildren = ObfuscationReflectionHelper.getPrivateValue(Screen.class, backpackScreen, "f_96540_");
		List<AbstractWidget> screenRenderables = ObfuscationReflectionHelper.getPrivateValue(Screen.class, backpackScreen, "f_169369_");
		if (screenChildren == null || screenRenderables == null) {
			return;
		}

		buttons.forEach(screenChildren::remove);
		buttons.forEach(screenRenderables::remove);
		buttons.clear();
	}

	@Override
	public int getWidth() {
		return 18;
	}

	@Override
	@OnlyIn(Dist.CLIENT)
	public void setBackpackScreen(BackpackScreen screen) {
		backpackScreen = screen;
	}

	@Override
	public void onCraftingSlotsDisplayed(List<Slot> slots) {
		if (slots.isEmpty()) {
			return;
		}
		Slot firstSlot = slots.get(0);
/* TODO readd when crafting tweaks is on 1.17
		addButton(CraftingTweaksAPI.createRotateButtonRelative(0, backpackScreen, getButtonX(firstSlot), getButtonY(firstSlot, 0)));
		addButton(CraftingTweaksAPI.createBalanceButtonRelative(0, backpackScreen, getButtonX(firstSlot), getButtonY(firstSlot, 1)));
		addButton(CraftingTweaksAPI.createClearButtonRelative(0, backpackScreen, getButtonX(firstSlot), getButtonY(firstSlot, 2)));
*/
	}

	@OnlyIn(Dist.CLIENT)
	private int getButtonX(Slot firstSlot) {
		return firstSlot.x - 19;
	}

	@OnlyIn(Dist.CLIENT)
	private int getButtonY(Slot firstSlot, int index) {
		return firstSlot.y + 18 * index;
	}
}
