package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.function.Consumer;

public class PickupUpgradeContainer extends UpgradeContainerBase {
	public static final UpgradeContainerType<PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(PickupUpgradeContainer::new);
	private final PickupUpgradeWrapper pickupWrapper;

	public PickupUpgradeContainer(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		pickupWrapper = new PickupUpgradeWrapper(upgrade, upgradeSaveHandler);
		ItemStackHandler filterHandler = pickupWrapper.getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> slots.add(new FilterSlotItemHandler(filterHandler, slot, -100, -100)));
	}

	@Override
	public UpgradeContainerType<?> getType() {
		return TYPE;
	}
}
