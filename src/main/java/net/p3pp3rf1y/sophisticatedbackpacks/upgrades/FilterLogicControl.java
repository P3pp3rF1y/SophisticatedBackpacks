package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;

import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.*;

@OnlyIn(Dist.CLIENT)
public class FilterLogicControl extends CompositeWidget<Widget> {
	protected final FilterLogicContainer container;
	private final Button[] showButtons;
	private final int height;
	private final int width;
	private final int slotsTopYOffset;
	private final int slotsPerRow;

	public FilterLogicControl(Position position, FilterLogicContainer filterLogicContainer, int slotsPerRow, Button... showButtons) {
		super(position);
		container = filterLogicContainer;
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

		width = slotsPerRow * 18;
		slotsTopYOffset = showButtons.length > 0 ? 21 : 0;
		height = container.getFilterSlots().size() / slotsPerRow * 18 + slotsTopYOffset;
	}

	private boolean shouldShow(Button button) {
		for (Button showButton : showButtons) {
			if (showButton == button) {
				return true;
			}
		}
		return false;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.renderSlotsBackground(minecraft, matrixStack, x, y + slotsTopYOffset, slotsPerRow, container.getFilterSlots().size() / slotsPerRow);
	}

	@Override
	public int getWidth() {
		return width;
	}

	@Override
	public int getHeight() {
		return height;
	}

	public static class Basic extends FilterLogicControl {
		public Basic(Position position, FilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, ALLOW_LIST);
		}
	}

	public static class Advanced extends FilterLogicControl {
		public Advanced(Position position, FilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, ALLOW_LIST, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}

	public void moveSlotsToView(int screenGuiLeft, int screenGuiTop) {
		int upgradeSlotNumber = 0;
		for (Slot slot : container.getFilterSlots()) {
			slot.xPos = x - screenGuiLeft + 1 + (upgradeSlotNumber % slotsPerRow) * 18;
			slot.yPos = y - screenGuiTop + slotsTopYOffset + 1 + (upgradeSlotNumber / slotsPerRow) * 18;
			upgradeSlotNumber++;
		}
	}

	public enum Button {
		ALLOW_LIST,
		PRIMARY_MATCH,
		DURABILITY,
		NBT
	}
}
