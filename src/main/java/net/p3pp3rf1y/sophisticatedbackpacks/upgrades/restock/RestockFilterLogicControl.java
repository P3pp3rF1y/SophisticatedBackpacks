package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeButton;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl.Button.*;

public abstract class RestockFilterLogicControl extends FilterLogicControl<RestockFilterLogic, RestockFilterLogicContainer> {
	public static final ButtonDefinition.Toggle<RestockFilterType> RESTOCK_FILTER_TYPE = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					RestockFilterType.ALLOW, GuiHelper.getButtonStateData(new UV(32, 32), translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					RestockFilterType.BLOCK, GuiHelper.getButtonStateData(new UV(48, 32), translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1)),
					RestockFilterType.BACKPACK, GuiHelper.getButtonStateData(new UV(112, 48), translUpgradeButton("restock_filter_type_inventory"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected RestockFilterLogicControl(Position position, RestockFilterLogicContainer filterLogicContainer, int slotsPerRow, Button... buttons) {
		super(position, filterLogicContainer, slotsPerRow, true, buttons);
		addChild(new ToggleButton<>(new Position(x, y), RESTOCK_FILTER_TYPE, button -> updateRestockFilterType(), container::getRestockFilterType));
	}

	private void updateRestockFilterType() {
		RestockFilterType next = container.getRestockFilterType().next();
		container.setRestockFilterType(next);

		container.getFilterSlots().forEach(slot -> slot.setEnabled(next != RestockFilterType.BACKPACK));
	}

	public static class Basic extends RestockFilterLogicControl {
		public Basic(Position position, RestockFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow);
		}
	}

	public static class Advanced extends RestockFilterLogicControl {
		public Advanced(Position position, RestockFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
