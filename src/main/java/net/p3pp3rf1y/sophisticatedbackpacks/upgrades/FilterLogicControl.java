package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicControlBase.Button.*;

public class FilterLogicControl<L extends FilterLogic, C extends FilterLogicContainer<L>>
		extends FilterLogicControlBase<L, FilterLogicContainer.FilterLogicSlot, C> {
	public FilterLogicControl(Position position, C filterLogicContainer, int slotsPerRow, Button... showButtons) {
		this(position, filterLogicContainer, slotsPerRow, showButtons.length > 0, showButtons);
	}

	protected FilterLogicControl(Position position, C filterLogicContainer, int slotsPerRow, boolean buttonsVisible, Button... showButtons) {
		super(filterLogicContainer, position, buttonsVisible, slotsPerRow, showButtons);
	}

	public static class Basic extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> {
		public Basic(Position position, FilterLogicContainer<FilterLogic> filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, ALLOW_LIST);
		}
	}

	public static class Advanced extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> {
		public Advanced(Position position, FilterLogicContainer<FilterLogic> filterLogicContainer, int slotsPerRow) {
			super(position, filterLogicContainer, slotsPerRow, ALLOW_LIST, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
