package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControl;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControlBase.MatchButton.*;

public abstract class DepositFilterLogicControl extends FilterLogicControl<DepositFilterLogic, DepositFilterLogicContainer> {
	public static final ButtonDefinition.Toggle<DepositFilterType> DEPOSIT_FILTER_TYPE = ButtonDefinitions.createToggleButtonDefinition(
			Map.of(
					DepositFilterType.ALLOW, GuiHelper.getButtonStateData(new UV(0, 0), SBPTranslationHelper.INSTANCE.translUpgradeButton("allow"), Dimension.SQUARE_16, new Position(1, 1)),
					DepositFilterType.BLOCK, GuiHelper.getButtonStateData(new UV(16, 0), SBPTranslationHelper.INSTANCE.translUpgradeButton("block"), Dimension.SQUARE_16, new Position(1, 1)),
					DepositFilterType.INVENTORY, GuiHelper.getButtonStateData(new UV(64, 16), SBPTranslationHelper.INSTANCE.translUpgradeButton("deposit_filter_type_inventory"), Dimension.SQUARE_16, new Position(1, 1))
			));

	protected DepositFilterLogicControl(StorageScreen<?> screen, Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow, MatchButton... matchButtons) {
		super(screen, position, filterLogicContainer, slotsPerRow, true, matchButtons);
		addChild(new ToggleButton<>(new Position(x, y), DEPOSIT_FILTER_TYPE, button -> updateDepositFilterType(), container::getDepositFilterType));
	}

	private void updateDepositFilterType() {
		DepositFilterType next = container.getDepositFilterType().next();
		container.setDepositFilterType(next);

		container.getFilterSlots().forEach(slot -> slot.setEnabled(next != DepositFilterType.INVENTORY));
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}

	public static class Basic extends DepositFilterLogicControl {
		public Basic(StorageScreen<?> screen, Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow);
		}
	}

	public static class Advanced extends DepositFilterLogicControl {
		public Advanced(StorageScreen<?> screen, Position position, DepositFilterLogicContainer filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
