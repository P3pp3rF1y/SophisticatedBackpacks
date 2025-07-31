package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.core.component.DataComponents;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.StatefulComponentItemHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class AnvilUpgradeWrapper extends UpgradeWrapperBase<AnvilUpgradeWrapper, AnvilUpgradeItem> {
	private final StatefulComponentItemHandler inventory;

	protected AnvilUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);

		inventory = new StatefulComponentItemHandler(upgrade, DataComponents.CONTAINER, 2) {
			@Override
			protected void onContentsChanged(int slot, ItemStack oldStack, ItemStack newStack) {
				super.onContentsChanged(slot, oldStack, newStack);
				save();
			}

			@Override
			public boolean isItemValid(int slot, ItemStack stack) {
				return true;
			}
		};
	}

	public StatefulComponentItemHandler getInventory() {
		return inventory;
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}

	public boolean shouldShiftClickIntoStorage() {
		return upgrade.getOrDefault(ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE, true);
	}

	public void setShiftClickIntoStorage(boolean shiftClickIntoStorage) {
		upgrade.set(ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE, shiftClickIntoStorage);
		save();
	}

	public String getItemName() {
		return upgrade.getOrDefault(ModDataComponents.ITEM_NAME, "");
	}

	public void setItemName(String itemName) {
		upgrade.set(ModDataComponents.ITEM_NAME, itemName);
		save();
	}
}
