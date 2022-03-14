package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.client.gui.narration.NarrationElementOutput;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;

import static net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicControlBase.MatchButton.*;

public abstract class ContentsFilterControl extends FilterLogicControl<ContentsFilterLogic, ContentsFilterLogicContainer> {

	protected ContentsFilterControl(StorageScreenBase<?> screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton, MatchButton... matchButtons) {
		super(screen, position, filterLogicContainer, slotsPerRow, true, matchButtons);
		addChild(new ToggleButton<>(new Position(x, y), contentsFilterButton, button -> updateFilterType(), container::getFilterType));
	}

	private void updateFilterType() {
		ContentsFilterType next = container.getFilterType().next();
		if (container.getPrimaryMatch() == PrimaryMatch.TAGS && next == ContentsFilterType.STORAGE) {
			next = next.next();
		}
		container.setFilterType(next);

		boolean slotsEnabled = next != ContentsFilterType.STORAGE;
		container.getFilterSlots().forEach(slot -> slot.setEnabled(slotsEnabled));
	}

	@Override
	protected void onTagsMatchSelected() {
		if (container.getFilterType() == ContentsFilterType.STORAGE) {
			updateFilterType();
		}
	}

	@Override
	public void updateNarration(NarrationElementOutput pNarrationElementOutput) {
		//TODO add narration
	}

	public static class Basic extends ContentsFilterControl {
		public Basic(StorageScreenBase<?> screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(screen, position, filterLogicContainer, slotsPerRow, contentsFilterButton);
		}
	}

	public static class Advanced extends ContentsFilterControl {
		public Advanced(StorageScreenBase<?> screen, Position position, ContentsFilterLogicContainer filterLogicContainer, int slotsPerRow, ButtonDefinition.Toggle<ContentsFilterType> contentsFilterButton) {
			super(screen, position, filterLogicContainer, slotsPerRow, contentsFilterButton, PRIMARY_MATCH, DURABILITY, NBT);
		}
	}
}
