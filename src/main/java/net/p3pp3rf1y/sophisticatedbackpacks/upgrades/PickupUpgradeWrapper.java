package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilterItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class PickupUpgradeWrapper {
	private final ItemStack upgrade;
	private final Consumer<ItemStack> upgradeSaveHandler;
	private FilterItemStackHandler filterHandler = null;

	public PickupUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		this.upgrade = upgrade;
		this.upgradeSaveHandler = upgradeSaveHandler;
	}

	public ItemStackHandler getFilterHandler() {
		if (filterHandler == null) {
			filterHandler = new FilterItemStackHandler(9) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					upgrade.setTagInfo("filters", serializeNBT());
					upgradeSaveHandler.accept(upgrade);
				}
			};
			NBTHelper.getCompound(upgrade, "filters").ifPresent(filterHandler::deserializeNBT);
		}

		return filterHandler;
	}
}
