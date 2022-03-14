package net.p3pp3rf1y.sophisticatedcore.upgrades.crafting;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;

public class CraftingUpgradeTab extends UpgradeSettingsTab<CraftingUpgradeContainer> {
	private static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, new UV(97, 216), new Dimension(15, 8));

	private final ICraftingUIPart craftingUIAddition;

	public CraftingUpgradeTab(CraftingUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen, ButtonDefinition.Toggle<Boolean> shiftClickTargetButton) {
		super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("crafting"), TranslationHelper.INSTANCE.translUpgradeTooltip("crafting"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), shiftClickTargetButton, button -> getContainer().setShiftClickIntoStorage(!getContainer().shouldShiftClickIntoStorage()),
				getContainer()::shouldShiftClickIntoStorage));
		craftingUIAddition = screen.getCraftingUIAddition();
		openTabDimension = new Dimension(63 + craftingUIAddition.getWidth(), 142);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			GuiHelper.renderSlotsBackground(matrixStack, x + 3 + craftingUIAddition.getWidth(), y + 44, 3, 3);
			GuiHelper.blit(matrixStack, x + 3 + craftingUIAddition.getWidth() + 19, y + 101, ARROW);
			GuiHelper.blit(matrixStack, x + 3 + craftingUIAddition.getWidth() + 14, y + 111, GuiHelper.CRAFTING_RESULT_SLOT);
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
			slot.x = x + 3 + craftingUIAddition.getWidth() - screen.getGuiLeft() + 1 + (slotNumber % 3) * 18;
			slot.y = y + 44 - screen.getGuiTop() + 1 + (slotNumber / 3) * 18;
			slotNumber++;
			if (slotNumber >= 9) {
				break;
			}
		}

		Slot craftingSlot = getContainer().getSlots().get(9);
		craftingSlot.x = x + 3 + craftingUIAddition.getWidth() - screen.getGuiLeft() + 19;
		craftingSlot.y = y + 44 - screen.getGuiTop() + 72;

		craftingUIAddition.onCraftingSlotsDisplayed(getContainer().getSlots());
	}
}
