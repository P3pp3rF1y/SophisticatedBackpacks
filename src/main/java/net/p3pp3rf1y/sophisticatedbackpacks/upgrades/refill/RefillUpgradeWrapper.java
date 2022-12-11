package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.capabilities.ForgeCapabilities;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

public class RefillUpgradeWrapper extends UpgradeWrapperBase<RefillUpgradeWrapper, RefillUpgradeItem>
		implements IFilteredUpgrade, ITickableUpgrade {
	private static final int COOLDOWN = 5;

	private final FilterLogic filterLogic;

	public RefillUpgradeWrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.refillUpgrade.filterSlots.get());
		filterLogic.setAllowByDefault();
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (entity == null /*not supported in block form*/ || isInCooldown(world)) {
			return;
		}
		entity.getCapability(ForgeCapabilities.ITEM_HANDLER, null).ifPresent(playerInvHandler -> InventoryHelper.iterate(filterLogic.getFilterHandler(), (slot, filter) -> {
			if (filter.isEmpty()) {
				return;
			}
			tryRefillFilter(entity, playerInvHandler, filter);
		}));
		setCooldown(world, COOLDOWN);
	}

	private void tryRefillFilter(@Nonnull LivingEntity entity, IItemHandler playerInvHandler, ItemStack filter) {
		int missingCount = InventoryHelper.getCountMissingInHandler(playerInvHandler, filter, filter.getMaxStackSize());
		if (entity instanceof Player player && ItemHandlerHelper.canItemStacksStack(player.containerMenu.getCarried(), filter)) {
			missingCount -= Math.min(missingCount, player.containerMenu.getCarried().getCount());
		}
		if (missingCount == 0) {
			return;
		}
		IItemHandler extractFromHandler = storageWrapper.getInventoryForUpgradeProcessing();
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
