package net.p3pp3rf1y.sophisticatedcore.upgrades.battery;

import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.Minecraft;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;

import java.util.List;

public class BatteryUpgradeTab extends UpgradeSettingsTab<BatteryUpgradeContainer> {
	public BatteryUpgradeTab(BatteryUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, TranslationHelper.INSTANCE.translUpgrade("battery"), TranslationHelper.INSTANCE.translUpgradeTooltip("battery"));
		openTabDimension = new Dimension(48, 48);
	}

	@Override
	protected void renderBg(PoseStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(matrixStack, minecraft, mouseX, mouseY);
		if (getContainer().isOpen()) {
			GuiHelper.renderSlotsBackground(matrixStack, x + 3, y + 24, 1, 1);
			GuiHelper.renderSlotsBackground(matrixStack, x + 24, y + 24, 1, 1);
		}
	}

	@Override
	protected void moveSlotsToTab() {
		List<Slot> slots = getContainer().getSlots();
		positionSlot(slots.get(BatteryUpgradeWrapper.INPUT_SLOT), screen.getGuiLeft(), screen.getGuiTop(), 4);
		positionSlot(slots.get(BatteryUpgradeWrapper.OUTPUT_SLOT), screen.getGuiLeft(), screen.getGuiTop(), 25);
	}

	private void positionSlot(Slot slot, int screenGuiLeft, int screenGuiTop, int xOffset) {
		slot.x = x - screenGuiLeft + xOffset;
		slot.y = y - screenGuiTop + 25;
	}
}
