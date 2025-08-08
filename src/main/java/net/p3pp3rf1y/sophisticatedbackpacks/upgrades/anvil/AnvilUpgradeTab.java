package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.util.ARGB;
import net.minecraft.util.FormattedCharSequence;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.TextBox;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.*;

import java.util.List;

public class AnvilUpgradeTab extends UpgradeSettingsTab<AnvilUpgradeContainer> {

	public static final TextureBlitData EDIT_ITEM_NAME_BACKGROUND_DISABLED = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(28, 115), new Dimension(100, 16));
	public static final TextureBlitData EDIT_ITEM_NAME_BACKGROUND = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(28, 99), new Dimension(100, 16));
	public static final TextureBlitData PLUS_SIGN = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(113, 203), new Dimension(13, 13));
	public static final TextureBlitData ARROW = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(56, 221), new Dimension(14, 15));
	public static final TextureBlitData RED_CROSS = new TextureBlitData(GuiHelper.GUI_CONTROLS, Dimension.SQUARE_256, new UV(113, 216), new Dimension(15, 15));
	private static final Component TOO_EXPENSIVE_TEXT = Component.translatable("container.repair.expensive");
	private final TextBox itemNameTextBox;

	public AnvilUpgradeTab(AnvilUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, BackpackTranslationHelper.INSTANCE.translUpgrade("anvil"), BackpackTranslationHelper.INSTANCE.translUpgradeTooltip("anvil"));
		openTabDimension = new Dimension(103, 92);
		itemNameTextBox = new TextBox(new Position(x + 5, y + 25), new Dimension(90, 14)) {
			@Override
			public boolean mouseClicked(double mouseX, double mouseY, int button) {
				if (isEditable()) {
					setFocused(true);
					screen.setFocused(itemNameTextBox);
				}
				return super.mouseClicked(mouseX, mouseY, button);
			}

			@Override
			protected void renderBg(GuiGraphics guiGraphics, Minecraft minecraft, int mouseX, int mouseY) {
				super.renderBg(guiGraphics, minecraft, mouseX, mouseY);
				TextureBlitData textureBlitData = getContainer().getSlots().get(0).hasItem() ? EDIT_ITEM_NAME_BACKGROUND : EDIT_ITEM_NAME_BACKGROUND_DISABLED;

				GuiHelper.blit(guiGraphics, getX() - 2, getY() - 2, textureBlitData, getWidth() + 4, getHeight() + 2);
			}
		};

		itemNameTextBox.setTextColor(-1);
		itemNameTextBox.setTextColorUneditable(-1);
		itemNameTextBox.setBordered(false);
		itemNameTextBox.setMaxLength(50);
		itemNameTextBox.setResponder(this::onNameChanged);
		itemNameTextBox.setValue(getContainer().getItemName());
		addHideableChild(itemNameTextBox);
		itemNameTextBox.setEditable(!upgradeContainer.getSlots().get(0).getItem().isEmpty());

		getContainer().setNameChangeListener(() -> {
			itemNameTextBox.setValue(getContainer().getItemName());
			itemNameTextBox.setEditable(!getContainer().getSlots().get(0).getItem().isEmpty());
		});
	}

	private void onNameChanged(String name) {
		if (getContainer().isProcessingOnTakeLogic()) {
			return;
		}
		getContainer().setItemName(name);
	}

	@Override
	protected void renderBg(GuiGraphics guiGraphics, Minecraft minecraft, int mouseX, int mouseY) {
		super.renderBg(guiGraphics, minecraft, mouseX, mouseY);

		if (getContainer().isOpen()) {
			renderSlotBg(guiGraphics, getContainer().getSlots().get(0));
			renderSlotBg(guiGraphics, getContainer().getSlots().get(1));
			renderSlotBg(guiGraphics, getContainer().getSlots().get(2));
		}
	}

	private void renderSlotBg(GuiGraphics guiGraphics, Slot slot) {
		GuiHelper.renderSlotsBackground(guiGraphics, slot.x + screen.getGuiLeft() - 1, slot.y + screen.getGuiTop() - 1, 1, 1);
	}

	@Override
	protected void renderWidget(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
		super.renderWidget(guiGraphics, mouseX, mouseY, partialTicks);

		if (!isOpen) {
			return;
		}

		renderCost(guiGraphics, x + 3, y + 62);

		Slot firstSlot = getContainer().getSlots().get(0);
		int inputSlotsY = firstSlot.y + screen.getGuiTop();
		int firstInputSlotX = firstSlot.x + screen.getGuiLeft();
		int secondInputSlotX = getContainer().getSlots().get(1).x + screen.getGuiLeft();
		Slot resultSlot = getContainer().getSlots().get(2);
		int resultSlotX = resultSlot.x + screen.getGuiLeft();

		GuiHelper.blit(guiGraphics, firstInputSlotX + 18 + (secondInputSlotX - (firstInputSlotX + 18)) / 2 - PLUS_SIGN.getWidth() / 2 - 1, inputSlotsY + 2, PLUS_SIGN);
		int arrowX = secondInputSlotX + 18 + (resultSlotX - (secondInputSlotX + 18)) / 2 - ARROW.getWidth() / 2 - 1;
		int arrowY = inputSlotsY + 1;
		GuiHelper.blit(guiGraphics, arrowX, arrowY, ARROW);

		if (firstSlot.hasItem() && !resultSlot.hasItem()) {
			GuiHelper.blit(guiGraphics, arrowX, arrowY, RED_CROSS);
		}
	}

	@Override
	protected void moveSlotsToTab() {
		Slot firstInputSlot = getContainer().getSlots().get(0);
		firstInputSlot.x = x - screen.getGuiLeft() + 4;
		firstInputSlot.y = y + 42 - screen.getGuiTop() + 1;

		Slot secondInputSlot = getContainer().getSlots().get(1);
		secondInputSlot.x = x - screen.getGuiLeft() + getWidth() / 2 - 9;
		secondInputSlot.y = y + 42 - screen.getGuiTop() + 1;

		Slot resultSlot = getContainer().getSlots().get(2);
		resultSlot.x = x - screen.getGuiLeft() + getWidth() - 2 - 3 - 18;
		resultSlot.y = y + 42 - screen.getGuiTop() + 1;
	}

	protected void renderCost(GuiGraphics guiGraphics, int x, int y) {
		int i = getContainer().getCost();
		if (i > 0) {
			int color = 8453920;
			Component component;
			if (i >= 40 && !minecraft.player.getAbilities().instabuild) {
				component = TOO_EXPENSIVE_TEXT;
				color = 16736352;
			} else if (!getContainer().getSlots().get(2).hasItem()) {
				component = null;
			} else {
				component = Component.translatable("container.repair.cost", i);
				if (!getContainer().getSlots().get(2).mayPickup(minecraft.player)) {
					color = 16736352;
				}
			}

			if (component != null) {
				int maxWidth = getWidth() - 9;
				List<FormattedCharSequence> lines = font.split(component, maxWidth);
				guiGraphics.fill(x, y, x + maxWidth, y + lines.size() * 12, 1325400064);

				int yOffset = 0;
				for (FormattedCharSequence line : lines) {
					int width = font.width(line);
					guiGraphics.drawString(font, line, x + 2 + (maxWidth - width) / 2, y + 2 + yOffset, ARGB.opaque(color), true);
					yOffset += 12;
				}
			}
		}
	}
}
