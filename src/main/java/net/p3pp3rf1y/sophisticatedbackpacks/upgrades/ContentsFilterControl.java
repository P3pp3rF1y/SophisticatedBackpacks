package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeButton;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public abstract class ContentsFilterControl extends FilterLogicControl<ContentsFilterLogic, ContentsFilterLogicContainer> {
	public static final ButtonDefinition.Toggle<ContentsFilterType> BACKPACK_CONTENTS_FILTER_TYPE = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					ContentsFilterType.ALLOW, GuiHelper.getButtonStateData(new UV(32, 32), translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					ContentsFilterType.BLOCK, GuiHelper.getButtonStateData(new UV(48, 32), translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1)),
					ContentsFilterType.BACKPACK, GuiHelper.getButtonStateData(new UV(112, 48), translUpgradeButton("match_backpack_contents"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected ContentsFilterControl(Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow, Button... buttons) {
		super(position, filterLogicContainer, slotsPerRow, true, buttons);
		addChild(new ToggleButton<>(new Position(x, y), BACKPACK_CONTENTS_FILTER_TYPE, button -> updateFilterType(), container::getFilterType));
	}

	private void updateFilterType() {
		ContentsFilterType next = container.getFilterType().next();
		container.setFilterType(next);

		container.getFilterSlots().forEach(slot -> slot.setEnabled(next != ContentsFilterType.BACKPACK));
	}

	public static class Basic extends ContentsFilterControl {
		public Basic(Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow);
		}
	}

	public static class Advanced extends ContentsFilterControl {
		public Advanced(Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
