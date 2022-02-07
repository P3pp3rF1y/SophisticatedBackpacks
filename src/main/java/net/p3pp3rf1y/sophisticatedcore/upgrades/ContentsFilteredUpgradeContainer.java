package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;

public class ContentsFilteredUpgradeContainer<W extends IUpgradeWrapper & IContentsFilteredUpgrade>
		extends UpgradeContainerBase<W, ContentsFilteredUpgradeContainer<W>> {
	private final ContentsFilterLogicContainer filterLogicContainer;

	public ContentsFilteredUpgradeContainer(Player player, int containerId, W wrapper, UpgradeContainerType<W, ContentsFilteredUpgradeContainer<W>> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new ContentsFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public ContentsFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		filterLogicContainer.handleMessage(data);
	}
}
