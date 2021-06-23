package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import com.google.common.collect.ImmutableMap;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControl;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translUpgradeButton;
import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public abstract class DepositFilterLogicControl extends FilterLogicControl<DepositFilterLogic, DepositFilterLogicContainer> {
	public static final ButtonDefinition.Toggle<DepositFilterType> DEPOSIT_FILTER_TYPE = ButtonDefinitions.createToggleButtonDefinition(
			ImmutableMap.of(
					DepositFilterType.ALLOW, GuiHelper.getButtonStateData(new UV(0, 0), translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					DepositFilterType.BLOCK, GuiHelper.getButtonStateData(new UV(16, 0), translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1)),
					DepositFilterType.INVENTORY, GuiHelper.getButtonStateData(new UV(64, 16), translUpgradeButton("deposit_filter_type_inventory"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected DepositFilterLogicControl(Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow, Button... buttons) {
		super(position, filterLogicContainer, slotsPerRow, true, buttons);
		addChild(new ToggleButton<>(new Position(x, y), DEPOSIT_FILTER_TYPE, button -> updateDepositFilterType(), container::getDepositFilterType));
	}

	private void updateDepositFilterType() {
		DepositFilterType next = container.getDepositFilterType().next();
		container.setDepositFilterType(next);

		container.getFilterSlots().forEach(slot -> slot.setEnabled(next != DepositFilterType.INVENTORY));
	}

	public static class Basic extends DepositFilterLogicControl {
		public Basic(Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow);
		}
	}

	public static class Advanced extends DepositFilterLogicControl {
		public Advanced(Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
