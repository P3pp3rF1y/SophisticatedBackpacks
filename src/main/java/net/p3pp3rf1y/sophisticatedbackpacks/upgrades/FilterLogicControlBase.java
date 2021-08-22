package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public abstract class FilterLogicControlBase<F extends FilterLogicBase, S extends Slot, C extends FilterLogicContainerBase<F, S>>
		extends CompositeWidget<Widget> {
	protected final Button[] showButtons;
	protected final int slotsTopYOffset;
	protected final int slotsPerRow;
	protected final int slotsInExtraRow;
	protected final int fullSlotRows;
	protected final C container;
	private final int height;
	private final int width;

	protected FilterLogicControlBase(C container, Position position, boolean buttonsVisible, int slotsPerRow, Button... showButtons) {
		super(position);
		this.container = container;
		slotsTopYOffset = buttonsVisible ? 21 : 0;
		this.slotsPerRow = slotsPerRow;
		this.showButtons = showButtons;

		if (shouldShow(ALLOW_LIST)) {
			addChild(new ToggleButton<>(new Position(x, y), ButtonDefinitions.ALLOW_LIST, button -> container.setAllowList(!container.isAllowList()), container::isAllowList));
		}
		if (shouldShow(PRIMARY_MATCH)) {
			addChild(new ToggleButton<>(new Position(x + 18, y), ButtonDefinitions.PRIMARY_MATCH,
					button -> container.setPrimaryMatch(container.getPrimaryMatch().next()), container::getPrimaryMatch));
		}
		if (shouldShow(DURABILITY)) {
			addChild(new ToggleButton<>(new Position(x + 36, y), ButtonDefinitions.MATCH_DURABILITY,
					button -> container.setMatchDurability(!container.shouldMatchDurability()), container::shouldMatchDurability));
		}
		if (shouldShow(NBT)) {
			addChild(new ToggleButton<>(new Position(x + 54, y), ButtonDefinitions.MATCH_NBT,
					button -> container.setMatchNbt(!container.shouldMatchNbt()), container::shouldMatchNbt));
		}

		width = Math.max(slotsPerRow * 18, getMaxButtonWidth());
		fullSlotRows = container.getFilterSlots().size() / slotsPerRow;
		slotsInExtraRow = container.getFilterSlots().size() % slotsPerRow;
		height = (fullSlotRows + (slotsInExtraRow > 0 ? 1 : 0)) * 18 + slotsTopYOffset;

	}

	protected int getMaxButtonWidth() {
		int maxWidth = 0;
		for (Widget w : children) {
			int buttonWidth = w.getX() + w.getWidth() - x;
			if (buttonWidth > maxWidth) {
				maxWidth = buttonWidth;
			}
		}
		return maxWidth;
	}

	protected boolean shouldShow(Button button) {
		for (Button showButton : showButtons) {
			if (showButton == button) {
				return true;
			}
		}
		return false;
	}

	public void moveSlotsToView(int screenGuiLeft, int screenGuiTop) {
		int upgradeSlotNumber = 0;
		for (S slot : container.getFilterSlots()) {
			slot.x = x - screenGuiLeft + 1 + (upgradeSlotNumber % slotsPerRow) * 18;
			slot.y = y - screenGuiTop + slotsTopYOffset + 1 + (upgradeSlotNumber / slotsPerRow) * 18;
			upgradeSlotNumber++;
		}
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x, y + slotsTopYOffset, slotsPerRow, fullSlotRows, slotsInExtraRow);
	}

	public enum Button {
		ALLOW_LIST,
		PRIMARY_MATCH,
		DURABILITY,
		NBT
	}
}
