package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class PickupUpgradeContainer extends UpgradeContainerBase {
	public static final UpgradeContainerType<PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(PickupUpgradeContainer::new);
	private final PickupUpgradeWrapper pickupWrapper;

	public PickupUpgradeContainer(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide) {
		super(upgrade, isClientSide);
		pickupWrapper = new PickupUpgradeWrapper(upgrade, upgradeSaveHandler);
		ItemStackHandler filterHandler = pickupWrapper.getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> slots.add(new FilterSlotItemHandler(filterHandler, slot, -100, -100)));
	}

	@Override
	public UpgradeContainerType<?> getType() {
		return TYPE;
	}

	public void setAllowList(boolean isAllowList) {
		pickupWrapper.setAllowList(isAllowList);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), "isAllowList", isAllowList));
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		setAllowList(data.getBoolean("isAllowList"));
	}

	public boolean isAllowList() {
		return pickupWrapper.isAllowList();
	}
}
