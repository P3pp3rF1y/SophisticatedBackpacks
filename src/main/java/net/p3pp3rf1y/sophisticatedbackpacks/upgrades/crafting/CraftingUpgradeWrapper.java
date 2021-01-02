package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class CraftingUpgradeWrapper extends UpgradeWrapperBase<CraftingUpgradeWrapper, CraftingUpgradeItem> {
	private final ItemStackHandler inventory;

	public CraftingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);

		inventory = new ItemStackHandler(9) {
			@Override
			protected void onContentsChanged(int slot) {
				super.onContentsChanged(slot);
				upgrade.setTagInfo("craftingInventory", serializeNBT());
				save();
			}
		};
		NBTHelper.getCompound(upgrade, "craftingInventory").ifPresent(inventory::deserializeNBT);
	}

	public ItemStackHandler getInventory() {
		return inventory;
	}
}
