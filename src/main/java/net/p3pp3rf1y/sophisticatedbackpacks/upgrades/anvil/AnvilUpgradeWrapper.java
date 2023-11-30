package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.function.Consumer;

public class AnvilUpgradeWrapper extends UpgradeWrapperBase<AnvilUpgradeWrapper, AnvilUpgradeItem> {
	private final ItemStackHandler inventory;
	protected AnvilUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);

		inventory = new ItemStackHandler(2) {
			@Override
			protected void onContentsChanged(int slot) {
				super.onContentsChanged(slot);
				upgrade.addTagElement("craftingInventory", serializeNBT());
				save();
			}
		};
		NBTHelper.getCompound(upgrade, "craftingInventory").ifPresent(inventory::deserializeNBT);
	}

	public ItemStackHandler getInventory() {
		return inventory;
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}

	public boolean shouldShiftClickIntoStorage() {
		return NBTHelper.getBoolean(upgrade, "shiftClickIntoStorage").orElse(true);
	}

	public void setShiftClickIntoStorage(boolean shiftClickIntoStorage) {
		NBTHelper.setBoolean(upgrade, "shiftClickIntoStorage", shiftClickIntoStorage);
		save();
	}

	public String getItemName() {
		return NBTHelper.getString(upgrade, "itemName").orElse("");
	}

	public void setItemName(String itemName) {
		NBTHelper.putString(upgrade.getOrCreateTag(), "itemName", itemName);
		save();
	}
}
