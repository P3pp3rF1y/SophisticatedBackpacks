package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeButton;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.MatchButton.*;

public abstract class ContentsFilterControl extends FilterLogicControl<ContentsFilterLogic, ContentsFilterLogicContainer> {
	public static final ButtonDefinition.Toggle<ContentsFilterType> BACKPACK_CONTENTS_FILTER_TYPE = ButtonDefinitions.createToggleButtonDefinition(
			Map.of(
					ContentsFilterType.ALLOW, GuiHelper.getButtonStateData(new UV(0, 0), translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					ContentsFilterType.BLOCK, GuiHelper.getButtonStateData(new UV(16, 0), translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1)),
					ContentsFilterType.BACKPACK, GuiHelper.getButtonStateData(new UV(80, 16), translUpgradeButton("match_backpack_contents"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected ContentsFilterControl(BackpackScreen screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow, MatchButton... matchButtons) {
		super(screen, position, filterLogicContainer, slotsPerRow, true, matchButtons);
		addChild(new ToggleButton<>(new Position(x, y), BACKPACK_CONTENTS_FILTER_TYPE, button -> updateFilterType(), container::getFilterType));
	}

	private void updateFilterType() {
		ContentsFilterType next = container.getFilterType().next();
		if (container.getPrimaryMatch() == PrimaryMatch.TAGS && next == ContentsFilterType.BACKPACK) {
			next = next.next();
		}
		container.setFilterType(next);

		boolean slotsEnabled = next != ContentsFilterType.BACKPACK;
		container.getFilterSlots().forEach(slot -> slot.setEnabled(slotsEnabled));
	}

	@Override
	protected void onTagsMatchSelected() {
		if (container.getFilterType() == ContentsFilterType.BACKPACK) {
			updateFilterType();
		}
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}

	public static class Basic extends ContentsFilterControl {
		public Basic(BackpackScreen screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow);
		}
	}

	public static class Advanced extends ContentsFilterControl {
		public Advanced(BackpackScreen screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
