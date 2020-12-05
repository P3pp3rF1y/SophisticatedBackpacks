package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class SmeltingUpgradeContainer extends UpgradeContainerBase<SmeltingUpgradeWrapper, SmeltingUpgradeContainer> {
	public static final UpgradeContainerType<SmeltingUpgradeWrapper, SmeltingUpgradeContainer> TYPE = new UpgradeContainerType<>(SmeltingUpgradeContainer::new);
	private final SmeltingLogicContainer smeltingLogicContainer;

	public SmeltingUpgradeContainer(int containerId, SmeltingUpgradeWrapper wrapper, boolean isClientSide, UpgradeContainerType<SmeltingUpgradeWrapper, SmeltingUpgradeContainer> type) {
		super(containerId, wrapper, isClientSide, type);
		smeltingLogicContainer = new SmeltingLogicContainer(() -> upgradeWrapper.getSmeltingLogic(), slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		//noop
	}

	public SmeltingLogicContainer getSmeltingLogicContainer() {
		return smeltingLogicContainer;
	}
}
