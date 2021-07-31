package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Direction;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class FeedingUpgradeWrapper extends UpgradeWrapperBase<FeedingUpgradeWrapper, FeedingUpgradeItem> implements ITickableUpgrade, IFilteredUpgrade {
	private static final int COOLDOWN = 100;
	private static final int FEEDING_RANGE = 3;
	private final FilterLogic filterLogic;

	public FeedingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.feedingUpgrade.filterSlots.get(), ItemStack::isEdible);
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (isInCooldown(world) || (entity != null && !(entity instanceof PlayerEntity))) {
			return;
		}

		if (entity == null) {
			world.getEntities(EntityType.PLAYER, new AxisAlignedBB(pos).inflate(FEEDING_RANGE), p -> true).forEach(p -> feedPlayer(p, world));
		} else {
			feedPlayer((PlayerEntity) entity, world);
		}

		setCooldown(world, COOLDOWN);
	}

	private void feedPlayer(PlayerEntity player, World world) {
		int hungerLevel = 20 - player.getFoodData().getFoodLevel();
		if (hungerLevel == 0) {
			return;
		}
		tryFeedingFoodFromBackpack(world, hungerLevel, player);
	}

	private void tryFeedingFoodFromBackpack(World world, int hungerLevel, PlayerEntity player) {
		boolean isHurt = player.getHealth() < player.getMaxHealth() - 0.1F;
		IItemHandlerModifiable inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		InventoryHelper.iterate(inventory, (slot, stack) -> {
			//noinspection ConstantConditions - isFood check makes sure that food isn't null
			if (stack.isEdible() && filterLogic.matchesFilter(stack) && ((stack.getItem().getFoodProperties().getNutrition() / 2) < hungerLevel || hungerLevel > 0 && isHurt)) {
				ItemStack containerItem = ForgeEventFactory.onItemUseFinish(player, stack.copy(), 0, stack.getItem().finishUsingItem(stack, world, player));
				inventory.setStackInSlot(slot, stack);
				if (!ItemStack.matches(containerItem, stack)) {
					//not handling the case where player doesn't have item handler cap as the player should always have it. if that changes in the future well I guess I fix it
					player.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, Direction.UP)
							.ifPresent(playerInventory -> InventoryHelper.insertOrDropItem(player, containerItem, inventory, playerInventory));
				}
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
