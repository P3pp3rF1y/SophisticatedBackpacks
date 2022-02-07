package net.p3pp3rf1y.sophisticatedcore.upgrades.filter;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IIOFilterUpgrade;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Optional;
import java.util.function.Consumer;

public class FilterUpgradeWrapper extends UpgradeWrapperBase<FilterUpgradeWrapper, FilterUpgradeItem> implements IContentsFilteredUpgrade, IIOFilterUpgrade {
	private final ContentsFilterLogic filterLogic;

	public FilterUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(), storageWrapper::getInventoryHandler, storageWrapper.getSettingsHandler().getTypeCategory(MemorySettingsCategory.class));
	}

	public void setDirection(Direction direction) {
		NBTHelper.setEnumConstant(upgrade, "direction", direction);
		save();
		storageWrapper.refreshInventoryForInputOutput();
	}

	public Direction getDirection() {
		return NBTHelper.getEnumConstant(upgrade, "direction", Direction::fromName).orElse(Direction.BOTH);
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
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
