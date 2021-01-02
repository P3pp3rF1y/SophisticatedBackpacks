package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IIOFilterUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.Optional;
import java.util.function.Consumer;

public class FilterUpgradeWrapper extends UpgradeWrapperBase<FilterUpgradeWrapper, FilterUpgradeItem> implements IFilteredUpgrade, IIOFilterUpgrade {
	private final FilterLogic filterLogic;

	public FilterUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	public void setDirection(Direction direction) {
		NBTHelper.setEnumConstant(upgrade, "direction", direction);
		save();
		backpackWrapper.refreshInventoryForInputOutput();
	}

	public Direction getDirection() {
		return NBTHelper.getEnumConstant(upgrade, "direction", Direction::fromName).orElse(Direction.BOTH);
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public Optional<FilterLogic> getInputFilter() {
		Direction direction = getDirection();
		return direction == Direction.INPUT || direction == Direction.BOTH ? Optional.of(getFilterLogic()) : Optional.empty();
	}

	@Override
	public Optional<FilterLogic> getOutputFilter() {
		Direction direction = getDirection();
		return direction == Direction.OUTPUT || direction == Direction.BOTH ? Optional.of(getFilterLogic()) : Optional.empty();
	}
}
