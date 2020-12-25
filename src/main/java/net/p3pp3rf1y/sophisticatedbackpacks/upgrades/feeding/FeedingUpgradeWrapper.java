package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class FeedingUpgradeWrapper extends UpgradeWrapperBase<FeedingUpgradeWrapper, FeedingUpgradeItem> implements ITickableUpgrade, IFilteredUpgrade {
	private static final int COOLDOWN = 100;
	private static final int FEEDING_RANGE = 3;
	private final FilterLogic filterLogic;

	public FeedingUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.feedingUpgrade.filterSlots.get(), ItemStack::isFood);
	}

	@Override
	public void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper) {
		if (isInCooldown(world)) {
			return;
		}

		if (player == null) {
			world.getEntitiesWithinAABB(EntityType.PLAYER, new AxisAlignedBB(pos).grow(FEEDING_RANGE), p -> true).forEach(p -> feedPlayer(p, world, wrapper));
		} else {
			feedPlayer(player, world, wrapper);
		}

		setCooldown(world, COOLDOWN);
	}

	private void feedPlayer(PlayerEntity player, World world, IBackpackWrapper wrapper) {
		int hungerLevel = 20 - player.getFoodStats().getFoodLevel();
		if (hungerLevel == 0) {
			return;
		}
		tryFeedingFoodFromBackpack(world, wrapper, hungerLevel, player);
	}

	private void tryFeedingFoodFromBackpack(World world, IBackpackWrapper wrapper, int hungerLevel, PlayerEntity player) {
		boolean isHurt = player.getHealth() < player.getMaxHealth() - 1;
		InventoryHelper.iterate(wrapper.getInceptionInventoryHandler(), (slot, stack) -> {
			//noinspection ConstantConditions - isFood check makes sure that food isn't null
			if (stack.isFood() && filterLogic.matchesFilter(stack) && ((stack.getItem().getFood().getHealing() / 2) < hungerLevel || hungerLevel > 0 && isHurt)) {
				player.onFoodEaten(world, stack.copy());
				wrapper.getInceptionInventoryHandler().extractItem(slot, 1, false);

				return true;
			}
			return false;
		}, () -> false, ret -> ret);
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}
}
