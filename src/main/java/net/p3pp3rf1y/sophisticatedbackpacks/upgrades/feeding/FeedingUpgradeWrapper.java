package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding;

import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.ActionResultType;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.AxisAlignedBB;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.event.ForgeEventFactory;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

public class FeedingUpgradeWrapper extends UpgradeWrapperBase<FeedingUpgradeWrapper, FeedingUpgradeItem> implements ITickableUpgrade, IFilteredUpgrade {
	private static final int COOLDOWN = 100;
	private static final int STILL_HUNGRY_COOLDOWN = 10;
	private static final int FEEDING_RANGE = 3;
	private final FilterLogic filterLogic;

	public FeedingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		filterLogic = new FilterLogic(upgrade, upgradeSaveHandler, upgradeItem.getFilterSlotCount(), ItemStack::isEdible);
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (isInCooldown(world) || (entity != null && !(entity instanceof PlayerEntity))) {
			return;
		}

		if (entity == null) {
			world.getEntities(EntityType.PLAYER, new AxisAlignedBB(pos).inflate(FEEDING_RANGE), p -> true).forEach(p -> feedPlayerAndGetHungry(p, world));
		} else {
			if (feedPlayerAndGetHungry((PlayerEntity) entity, world)) {
				setCooldown(world, STILL_HUNGRY_COOLDOWN);
				return;
			}
		}

		setCooldown(world, COOLDOWN);
	}

	private boolean feedPlayerAndGetHungry(PlayerEntity player, World world) {
		int hungerLevel = 20 - player.getFoodData().getFoodLevel();
		if (hungerLevel == 0) {
			return false;
		}
		return tryFeedingFoodFromBackpack(world, hungerLevel, player) && player.getFoodData().getFoodLevel() < 20;
	}

	private boolean tryFeedingFoodFromBackpack(World world, int hungerLevel, PlayerEntity player) {
		boolean isHurt = player.getHealth() < player.getMaxHealth() - 0.1F;
		IItemHandlerModifiable inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		AtomicBoolean fedPlayer = new AtomicBoolean(false);
		InventoryHelper.iterate(inventory, (slot, stack) -> {
			if (stack.isEdible() && filterLogic.matchesFilter(stack) && (isHungryEnoughForFood(hungerLevel, stack) || shouldFeedImmediatelyWhenHurt() && hungerLevel > 0 && isHurt)) {
				ItemStack mainHandItem = player.getMainHandItem();
				player.inventory.items.set(player.inventory.selected, stack);
				if (stack.use(world, player, Hand.MAIN_HAND).getResult() == ActionResultType.CONSUME) {
					player.inventory.items.set(player.inventory.selected, mainHandItem);
					ItemStack containerItem = ForgeEventFactory.onItemUseFinish(player, stack.copy(), 0, stack.getItem().finishUsingItem(stack, world, player));
					inventory.setStackInSlot(slot, stack);
					if (!ItemStack.matches(containerItem, stack)) {
						//not handling the case where player doesn't have item handler cap as the player should always have it. if that changes in the future well I guess I fix it
						player.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY, Direction.UP)
								.ifPresent(playerInventory -> InventoryHelper.insertOrDropItem(player, containerItem, inventory, playerInventory));
					}
					fedPlayer.set(true);
					return true;
				}
				player.inventory.items.set(player.inventory.selected, mainHandItem);
			}
			return false;
		}, () -> false, ret -> ret);
		return fedPlayer.get();
	}

	private boolean isHungryEnoughForFood(int hungerLevel, ItemStack stack) {
		HungerLevel feedAtHungerLevel = getFeedAtHungerLevel();
		if (feedAtHungerLevel == HungerLevel.ANY) {
			return true;
		}

		//noinspection ConstantConditions - isFood check makes sure that food isn't null
		int nutrition = stack.getItem().getFoodProperties().getNutrition();
		return (feedAtHungerLevel == HungerLevel.HALF ? (nutrition / 2) : nutrition) <= hungerLevel;
	}

	@Override
	public FilterLogic getFilterLogic() {
		return filterLogic;
	}

	public HungerLevel getFeedAtHungerLevel() {
		return NBTHelper.getEnumConstant(upgrade, "feedAtHungerLevel", HungerLevel::fromName).orElse(HungerLevel.HALF);
	}

	public void setFeedAtHungerLevel(HungerLevel hungerLevel) {
		NBTHelper.setEnumConstant(upgrade, "feedAtHungerLevel", hungerLevel);
		save();
	}

	public boolean shouldFeedImmediatelyWhenHurt() {
		return NBTHelper.getBoolean(upgrade, "feedImmediatelyWhenHurt").orElse(true);
	}

	public void setFeedImmediatelyWhenHurt(boolean feedImmediatelyWhenHurt) {
		NBTHelper.setBoolean(upgrade, "feedImmediatelyWhenHurt", feedImmediatelyWhenHurt);
		save();
	}
}
