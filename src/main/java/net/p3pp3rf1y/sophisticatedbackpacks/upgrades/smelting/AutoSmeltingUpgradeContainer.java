package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;

public class AutoSmeltingUpgradeContainer extends UpgradeContainerBase<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeContainer> {
	public static final UpgradeContainerType<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeContainer> TYPE = new UpgradeContainerType<>(AutoSmeltingUpgradeContainer::new);

	private final FilterLogicContainer<FilterLogic> inputFilterLogicContainer;

	private final FilterLogicContainer<FilterLogic> fuelFilterLogicContainer;
	private final SmeltingLogicContainer smeltingLogicContainer;

	public AutoSmeltingUpgradeContainer(Player player, int containerId, AutoSmeltingUpgradeWrapper wrapper, UpgradeContainerType<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeContainer> type) {
		super(player, containerId, wrapper, type);
		inputFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getInputFilterLogic(), this, slots::add);
		fuelFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFuelFilterLogic(), this, slots::add);
		smeltingLogicContainer = new SmeltingLogicContainer(() -> upgradeWrapper.getSmeltingLogic(), slots::add);
	}

	@Override
	public void handleMessage(CompoundTag data) {
		inputFilterLogicContainer.handleMessage(data);
	}

	public SmeltingLogicContainer getSmeltingLogicContainer() {
		return smeltingLogicContainer;
	}

	public FilterLogicContainer<FilterLogic> getInputFilterLogicContainer() {
		return inputFilterLogicContainer;
	}

	public FilterLogicContainer<FilterLogic> getFuelFilterLogicContainer() {
		return fuelFilterLogicContainer;
	}
}
