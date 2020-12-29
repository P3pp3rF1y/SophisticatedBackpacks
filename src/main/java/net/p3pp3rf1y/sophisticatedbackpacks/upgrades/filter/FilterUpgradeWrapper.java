package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class FilterUpgradeWrapper extends UpgradeWrapperBase<FilterUpgradeWrapper, FilterUpgradeItem> implements IFilteredUpgrade {
	private final FilterLogic filterLogic;

	public FilterUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	public void setDirection(Direction direction) {
		NBTHelper.setEnumConstant(upgrade, "direction", direction);
		save();
	}

	public Direction getDirection() {
		return NBTHelper.getEnumConstant(upgrade, "direction", Direction::fromName).orElse(Direction.BOTH);
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void onNbtChange(IBackpackWrapper backpackWrapper) {
		backpackWrapper.refreshInventoryForInputOutput();
	}
}
