package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ISlotChangeResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class VoidUpgradeWrapper extends UpgradeWrapperBase<VoidUpgradeWrapper, VoidUpgradeItem>
		implements IPickupResponseUpgrade, IInsertResponseUpgrade, IFilteredUpgrade, ISlotChangeResponseUpgrade, ITickableUpgrade {
	private final FilterLogic filterLogic;
	private final Set<Integer> slotsToVoid = new HashSet<>();

	public VoidUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
		filterLogic.setAllowByDefault();
	}

	@Override
	public ItemStack pickup(Level world, ItemStack stack, boolean simulate) {
		if (filterLogic.matchesFilter(stack)) {
			return ItemStack.EMPTY;
		}
		return stack;
	}

	@Override
	public ItemStack onBeforeInsert(IItemHandlerSimpleInserter inventoryHandler, int slot, ItemStack stack, boolean simulate) {
		if (filterLogic.matchesFilter(stack)) {
			return ItemStack.EMPTY;
		}
		return stack;
	}

	@Override
	public void onAfterInsert(IItemHandlerSimpleInserter inventoryHandler, int slot) {
		//noop
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	public void setShouldWorkdInGUI(boolean shouldWorkdInGUI) {
		NBTHelper.setBoolean(upgrade, "shouldWorkInGUI", shouldWorkdInGUI);
		save();
	}

	public boolean shouldWorkInGUI() {
		return NBTHelper.getBoolean(upgrade, "shouldWorkInGUI").orElse(false);
	}

	@Override
	public void onSlotChange(IItemHandler inventoryHandler, int slot) {
		if (!shouldWorkInGUI()) {
			return;
		}

		ItemStack slotStack = inventoryHandler.getStackInSlot(slot);
		if (filterLogic.matchesFilter(slotStack)) {
			slotsToVoid.add(slot);
		}
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (slotsToVoid.isEmpty()) {
			return;
		}

		BackpackInventoryHandler backpackInventory = backpackWrapper.getInventoryHandler();
		for (int slot : slotsToVoid) {
			backpackInventory.extractItem(slot, backpackInventory.getStackInSlot(slot).getCount(), false);
		}

		slotsToVoid.clear();
	}
}
