package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.core.component.DataComponents;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ComponentItemStacksHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class AnvilUpgradeWrapper extends UpgradeWrapperBase<AnvilUpgradeWrapper, AnvilUpgradeItem> {
	private final ComponentItemStacksHandler inventory;

	protected AnvilUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
		if (upgrade.has(DataComponents.CONTAINER)) {
			upgrade.set(ModCoreDataComponents.LENIENT_CONTAINER, upgrade.get(DataComponents.CONTAINER));
		}
		upgrade.remove(DataComponents.CONTAINER);
		inventory = new ComponentItemStacksHandler(upgrade, ModCoreDataComponents.LENIENT_CONTAINER.get(), 2) {
			@Override
			protected void onContentsChanged(int index, ItemStack previousContents) {
				super.onContentsChanged(index, previousContents);
				save();
			}

			@Override
			public boolean isValid(int slot, ItemResource resource) {
				return true;
			}
		};
	}

	public ComponentItemStacksHandler getInventory() {
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
