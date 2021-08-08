package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.item.ItemEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.function.Consumer;

public class MagnetUpgradeWrapper extends UpgradeWrapperBase<MagnetUpgradeWrapper, MagnetUpgradeItem>
		implements IContentsFilteredUpgrade, ITickableUpgrade {
	private static final String PREVENT_REMOTE_MOVEMENT = "PreventRemoteMovement";
	private static final String ALLOW_MACHINE_MOVEMENT = "AllowMachineRemoteMovement";

	private static final int BACKPACK_FILTER_REFRESH_COOLDOWN_TICKS = 10;
	private static final int COOLDOWN_TICKS = 10;
	private static final int FULL_COOLDOWN_TICKS = 40;
	private final ContentsFilterLogic filterLogic;

	private long backpackContentsRefreshCooldown = 0;

	private static final Set<IMagnetPreventionChecker> magnetCheckers = new HashSet<>();

	public static void addMagnetPreventionChecker(IMagnetPreventionChecker checker) {
		magnetCheckers.add(checker);
	}

	public MagnetUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new ContentsFilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount());
	}

	@Override
	public ContentsFilterLogic getFilterLogic() {
		return filterLogic;
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}

		List<ItemEntity> itemEntities = world.getEntities(EntityType.ITEM, new AxisAlignedBB(pos).inflate(upgradeItem.getRadius()), e -> true);
		if (itemEntities.isEmpty()) {
			setCooldown(world, COOLDOWN_TICKS);
			return;
		}

		int cooldown = FULL_COOLDOWN_TICKS;

		if (backpackContentsRefreshCooldown < world.getGameTime()) {
			backpackContentsRefreshCooldown = world.getGameTime() + BACKPACK_FILTER_REFRESH_COOLDOWN_TICKS;
			filterLogic.refreshBackpackFilterStacks(backpackWrapper.getInventoryForUpgradeProcessing());
		}

		for (ItemEntity itemEntity : itemEntities) {
			if (!itemEntity.isAlive() || !filterLogic.matchesFilter(itemEntity.getItem()) || canNotPickup(itemEntity, entity)) {
				continue;
			}
			if (tryToInsertItem(itemEntity)) {
				cooldown = COOLDOWN_TICKS;
			}
		}
		setCooldown(world, cooldown);
	}

	private boolean isBlockedBySomething(ItemEntity itemEntity) {
		for (IMagnetPreventionChecker checker : magnetCheckers) {
			if (checker.isBlocked(itemEntity)) {
				return true;
			}
		}
		return false;
	}

	private boolean canNotPickup(ItemEntity itemEntity, @Nullable LivingEntity player) {
		if (isBlockedBySomething(itemEntity)) {
			return true;
		}

		CompoundNBT data = itemEntity.getPersistentData();
		return player != null ? data.contains(PREVENT_REMOTE_MOVEMENT) : data.contains(PREVENT_REMOTE_MOVEMENT) && !data.contains(ALLOW_MACHINE_MOVEMENT);
	}

	private boolean tryToInsertItem(ItemEntity itemEntity) {
		ItemStack stack = itemEntity.getItem();
		IItemHandlerModifiable inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		ItemStack remaining = InventoryHelper.insertIntoInventory(stack, inventory, true);
		boolean insertedSomething = false;
		if (remaining.getCount() != stack.getCount()) {
			insertedSomething = true;
			remaining = InventoryHelper.insertIntoInventory(stack, inventory, false);
			itemEntity.setItem(remaining);
		}
		return insertedSomething;
	}
}
