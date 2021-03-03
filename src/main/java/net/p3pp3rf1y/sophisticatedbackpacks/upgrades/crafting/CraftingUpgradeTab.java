package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;

public class CraftingUpgradeTab extends UpgradeSettingsTab<CraftingUpgradeContainer> {
	private static final TextureBlitData CRAFTING_SLOT = new TextureBlitData(GuiHelper.GUI_CONTROLS, new UV(71, 216), new Dimension(26, 26));
	private static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, new UV(97, 216), new Dimension(15, 8));

	private final ICraftingUIPart craftingUIAddition;

	public static final ButtonDefinition.Toggle<Boolean> SHIFT_CLICK_TARGET = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					true, GuiHelper.getButtonStateData(new UV(64, 80), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("shift_click_into_backpack"), null)),
					false, GuiHelper.getButtonStateData(new UV(80, 80), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("shift_click_into_inventory")))
			));

	public CraftingUpgradeTab(CraftingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("crafting")),
				new TranslationTextComponent(translUpgradeTooltip("crafting")));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), SHIFT_CLICK_TARGET, button -> getContainer().setShiftClickIntoBackpack(!getContainer().shouldShiftClickIntoBackpack()),
				getContainer()::shouldShiftClickIntoBackpack));
		craftingUIAddition = screen.getCraftingUIAddition();
		openTabDimension = new Dimension(63 + craftingUIAddition.getWidth(), 142);
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			GuiHelper.renderSlotsBackground(minecraft, matrixStack, x + 3 + craftingUIAddition.getWidth(), y + 44, 3, 3);
			GuiHelper.blit(minecraft, matrixStack, x + 3 + craftingUIAddition.getWidth() + 19, y + 101, ARROW);
			GuiHelper.blit(minecraft, matrixStack, x + 3 + craftingUIAddition.getWidth() + 14, y + 111, CRAFTING_SLOT);
		}
	}

	@Override
	protected void onTabClose() {
		super.onTabClose();
		craftingUIAddition.onCraftingSlotsHidden();
	}

	@Override
	protected void moveSlotsToTab() {
		int slotNumber = 0;
		for (Slot slot : getContainer().getSlots()) {
			slot.xPos = x + 3 + craftingUIAddition.getWidth() - screen.getGuiLeft() + 1 + (slotNumber % 3) * 18;
			slot.yPos = y + 44 - screen.getGuiTop() + 1 + (slotNumber / 3) * 18;
			slotNumber++;
			if (slotNumber >= 9) {
				break;
			}
		}

		Slot craftingSlot = getContainer().getSlots().get(9);
		craftingSlot.xPos = x + 3 + craftingUIAddition.getWidth() - screen.getGuiLeft() + 19;
		craftingSlot.yPos = y + 44 - screen.getGuiTop() + 72;

		craftingUIAddition.onCraftingSlotsDisplayed(getContainer().getSlots());
	}
}
