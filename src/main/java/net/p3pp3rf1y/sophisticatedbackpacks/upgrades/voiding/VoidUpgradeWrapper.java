package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IInsertResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IOverflowResponseUpgrade;
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
		implements IPickupResponseUpgrade, IInsertResponseUpgrade, IFilteredUpgrade, ISlotChangeResponseUpgrade, ITickableUpgrade, IOverflowResponseUpgrade {
	private final FilterLogic filterLogic;
	private final Set<Integer> slotsToVoid = new HashSet<>();
	private boolean shouldVoidOverflow;

	public VoidUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
		filterLogic.setAllowByDefault();
		shouldVoidOverflow = NBTHelper.getBoolean(upgrade, "shouldVoidOverflow").orElse(false);
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, boolean simulate) {
		if (filterLogic.matchesFilter(stack)) {
			return ItemStack.EMPTY;
		}
		return stack;
	}

	@Override
	public ItemStack onBeforeInsert(IItemHandlerSimpleInserter inventoryHandler, int slot, ItemStack stack, boolean simulate) {
		return !shouldVoidOverflow && filterLogic.matchesFilter(stack) ? ItemStack.EMPTY : stack;
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

	public void setShouldVoidOverflow(boolean shouldVoidOverflow) {
		this.shouldVoidOverflow = shouldVoidOverflow;
		NBTHelper.setBoolean(upgrade, "shouldVoidOverflow", shouldVoidOverflow);
		save();
	}

	public boolean shouldVoidOverflow() {
		return shouldVoidOverflow;
	}

	@Override
	public void onSlotChange(IItemHandler inventoryHandler, int slot) {
		if (!shouldWorkInGUI() || shouldVoidOverflow()) {
			return;
		}

		ItemStack slotStack = inventoryHandler.getStackInSlot(slot);
		if (filterLogic.matchesFilter(slotStack)) {
			slotsToVoid.add(slot);
		}
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (slotsToVoid.isEmpty()) {
			return;
		}

		BackpackInventoryHandler backpackInventory = backpackWrapper.getInventoryHandler();
		for (int slot : slotsToVoid) {
			backpackInventory.extractItem(slot, backpackInventory.getStackInSlot(slot).getCount(), false);
		}

		slotsToVoid.clear();
	}

	@Override
	public boolean worksInGui() {
		return shouldWorkInGUI();
	}

	@Override
	public ItemStack onOverflow(ItemStack stack) {
		return filterLogic.matchesFilter(stack) ? ItemStack.EMPTY : stack;
	}
}
