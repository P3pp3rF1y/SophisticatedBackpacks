package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public class CraftingUpgradeTab extends UpgradeSettingsTab<CraftingUpgradeContainer> {
	private static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, new UV(97, 216), new Dimension(15, 8));

	private final ICraftingUIPart craftingUIAddition;

	public CraftingUpgradeTab(CraftingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("crafting")),
				new TranslationTextComponent(translUpgradeTooltip("crafting")));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), ButtonDefinitions.SHIFT_CLICK_TARGET, button -> getContainer().setShiftClickIntoBackpack(!getContainer().shouldShiftClickIntoBackpack()),
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
			GuiHelper.blit(minecraft, matrixStack, x + 3 + craftingUIAddition.getWidth() + 14, y + 111, GuiHelper.CRAFTING_RESULT_SLOT);
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
