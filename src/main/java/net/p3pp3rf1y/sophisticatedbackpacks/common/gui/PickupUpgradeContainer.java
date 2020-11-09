package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilterItemStackHandler;

public class PickupUpgradeContainer extends UpgradeContainerBase {
	public static final UpgradeContainerType<PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(PickupUpgradeContainer::new);

	public PickupUpgradeContainer(ItemStack upgrade) {
		super(upgrade);
		int size = 9;
		ItemStackHandler itemHandler = new FilterItemStackHandler(size);
		for (int i = 0; i < 9; i++) {
			slots.add(new FilterSlotItemHandler(itemHandler, i, -100, -100));
		}
	}

	@Override
	public UpgradeContainerType<?> getType() {
		return TYPE;
	}
}
