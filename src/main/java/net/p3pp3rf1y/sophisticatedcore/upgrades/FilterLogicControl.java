package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreen;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControlBase.MatchButton.*;

public class FilterLogicControl<L extends FilterLogic, C extends FilterLogicContainer<L>>
		extends FilterLogicControlBase<L, FilterLogicContainer.FilterLogicSlot, C> {
	public FilterLogicControl(StorageScreen<?> screen, Position position, C filterLogicContainer, int slotsPerRow, MatchButton... showMatchButtons) {
		this(screen, position, filterLogicContainer, slotsPerRow, showMatchButtons.length > 0, showMatchButtons);
	}

	protected FilterLogicControl(StorageScreen<?> screen, Position position, C filterLogicContainer, int slotsPerRow, boolean buttonsVisible, MatchButton... showMatchButtons) {
		super(screen, filterLogicContainer, position, buttonsVisible, slotsPerRow, showMatchButtons);
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}

	public static class Basic extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> {
		public Basic(StorageScreen<?> screen, Position position, FilterLogicContainer<FilterLogic> filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow, ALLOW_LIST);
		}
	}

	public static class Advanced extends FilterLogicControl<FilterLogic, FilterLogicContainer<FilterLogic>> {
		public Advanced(StorageScreen<?> screen, Position position, FilterLogicContainer<FilterLogic> filterLogicContainer, int slotsPerRow) {
			super(screen, position, filterLogicContainer, slotsPerRow, ALLOW_LIST, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
