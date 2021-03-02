package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class ContentsFilteredUpgradeContainer<W extends IUpgradeWrapper & IContentsFilteredUpgrade>
		extends UpgradeContainerBase<W, ContentsFilteredUpgradeContainer<W>> {
	private final ContentsFilterLogicContainer filterLogicContainer;

	public ContentsFilteredUpgradeContainer(PlayerEntity player, int containerId, W wrapper, UpgradeContainerType<W, ContentsFilteredUpgradeContainer<W>> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new ContentsFilterLogicContainer(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public ContentsFilterLogicContainer getFilterLogicContainer() {
		return filterLogicContainer;
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		filterLogicContainer.handleMessage(data);
	}
}
