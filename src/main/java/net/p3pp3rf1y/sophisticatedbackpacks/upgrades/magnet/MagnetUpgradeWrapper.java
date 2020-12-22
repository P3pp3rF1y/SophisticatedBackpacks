package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.List;
import java.util.function.Consumer;

public class MagnetUpgradeWrapper extends UpgradeWrapperBase<MagnetUpgradeWrapper, MagnetUpgradeItem> implements IFilteredUpgrade, ITickableUpgrade {
	private static final int COOLDOWN_TICKS = 10;
	private static final int FULL_COOLDOWN_TICKS = 40;
	private final FilterLogic filterLogic;

	public MagnetUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper) {
		if (isInCooldown(world)) {
			return;
		}

		List<ItemEntity> itemEntities = world.getEntitiesWithinAABB(EntityType.ITEM, new AxisAlignedBB(pos).grow(upgradeItem.getRadius()), e -> true);
		if (itemEntities.isEmpty()) {
			setCooldown(world, COOLDOWN_TICKS);
			return;
		}

		int cooldown = FULL_COOLDOWN_TICKS;
		for (ItemEntity itemEntity : itemEntities) {
			if (!filterLogic.matchesFilter(itemEntity.getItem())) {
				continue;
			}
			if (tryToInsertItem(wrapper, itemEntity)) {
				cooldown = COOLDOWN_TICKS;
			}
		}
		setCooldown(world, cooldown);
	}

	private boolean tryToInsertItem(IBackpackWrapper wrapper, ItemEntity itemEntity) {
		ItemStack stack = itemEntity.getItem();
		ItemStack remaining = InventoryHelper.insertIntoInventory(stack, wrapper.getInceptionInventoryHandler(), true);
		boolean insertedSomething = false;
		if (remaining.getCount() != stack.getCount()) {
			insertedSomething = true;
			remaining = InventoryHelper.insertIntoInventory(stack, wrapper.getInceptionInventoryHandler(), false);
			if (remaining.isEmpty()) {
				itemEntity.remove();
			} else {
				itemEntity.setItem(remaining);
			}
		}
		return insertedSomething;
	}
}
