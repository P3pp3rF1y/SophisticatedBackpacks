package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

public class RefillUpgradeWrapper extends UpgradeWrapperBase<RefillUpgradeWrapper, RefillUpgradeItem>
		implements IFilteredUpgrade, ITickableUpgrade {
	private static final int COOLDOWN = 5;

	private final FilterLogic filterLogic;

	public RefillUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.refillUpgrade.filterSlots.get());
		filterLogic.setAllowByDefault();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (entity == null /*not supported in block form*/ || isInCooldown(world)) {
			return;
		}
		entity.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, null).ifPresent(playerInvHandler -> InventoryHelper.iterate(filterLogic.getFilterHandler(), (slot, filter) -> {
			if (filter.isEmpty()) {
				return;
			}
			int missingCount = InventoryHelper.getCountMissingInHandler(playerInvHandler, filter, filter.getMaxStackSize());
			if (missingCount == 0) {
				return;
			}
			IItemHandler extractFromHandler = backpackWrapper.getInventoryForUpgradeProcessing();
			ItemStack toMove = filter.copy();
			toMove.setCount(missingCount);
			ItemStack extracted = InventoryHelper.extractFromInventory(toMove, extractFromHandler, true);
			if (extracted.isEmpty()) {
				return;
			}
			if (missingCount < filter.getMaxStackSize()) {
				refillExistingStack(playerInvHandler, extractFromHandler, extracted);
			} else {
				refillAnywhereInInventory(playerInvHandler, extractFromHandler, toMove, extracted);
			}
		}));
		setCooldown(world, COOLDOWN);
	}

	private void refillExistingStack(IItemHandler playerInvHandler, IItemHandler extractFromHandler, ItemStack extracted) {
		AtomicBoolean refilledStack = new AtomicBoolean(false);
		InventoryHelper.iterate(playerInvHandler, (slot, stack) -> {
			if (ItemHandlerHelper.canItemStacksStack(stack, extracted)) {
				playerInvHandler.insertItem(slot, InventoryHelper.extractFromInventory(extracted, extractFromHandler, false), false);
				refilledStack.set(true);
			}
		}, refilledStack::get);
	}

	private void refillAnywhereInInventory(IItemHandler playerInvHandler, IItemHandler extractFromHandler, ItemStack toMove, ItemStack extracted) {
		ItemStack remaining = InventoryHelper.insertIntoInventory(extracted, playerInvHandler, true);
		if (remaining.getCount() == extracted.getCount()) {
			return;
		}
		toMove.setCount(extracted.getCount() - remaining.getCount());
		InventoryHelper.insertIntoInventory(InventoryHelper.extractFromInventory(toMove, extractFromHandler, false), playerInvHandler, false);
	}
}
