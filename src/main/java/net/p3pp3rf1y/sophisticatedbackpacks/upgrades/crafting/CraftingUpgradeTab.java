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

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgrade;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeTooltip;

public class CraftingUpgradeTab extends UpgradeSettingsTab<CraftingUpgradeContainer> {
	private static final TextureBlitData GRID_BACKGROUND = new TextureBlitData(GuiHelper.BACKPACK_54, new UV(7, 17), new Dimension(54, 54));
	private static final TextureBlitData CRAFTING_SLOT = new TextureBlitData(GuiHelper.UPGRADE_CONTROLS, new UV(71, 216), new Dimension(26, 26));
	private static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.UPGRADE_CONTROLS, new UV(97, 216), new Dimension(15, 8));

	public CraftingUpgradeTab(CraftingUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, new Dimension(63, 120), screen, new TranslationTextComponent(translUpgrade("crafting")),
				new TranslationTextComponent(translUpgradeTooltip("crafting")));
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			GuiHelper.blit(minecraft, matrixStack, x + 3, y + 22, GRID_BACKGROUND);
			GuiHelper.blit(minecraft, matrixStack, x + 3 + 19, y + 79, ARROW);
			GuiHelper.blit(minecraft, matrixStack, x + 3 + 14, y + 89, CRAFTING_SLOT);
		}
	}

	@Override
	protected void moveSlotsToTab() {
		int slotNumber = 0;
		for (Slot slot : getContainer().getSlots()) {
			slot.xPos = x + 3 - screen.getGuiLeft() + 1 + (slotNumber % 3) * 18;
			slot.yPos = y + 22 - screen.getGuiTop() + 1 + (slotNumber / 3) * 18;
			slotNumber++;
			if (slotNumber >= 9) {
				break;
			}
		}

		Slot craftingSlot = getContainer().getSlots().get(9);
		craftingSlot.xPos = x + 3 - screen.getGuiLeft() + 19;
		craftingSlot.yPos = y + 22 - screen.getGuiTop() + 72;
	}
}
