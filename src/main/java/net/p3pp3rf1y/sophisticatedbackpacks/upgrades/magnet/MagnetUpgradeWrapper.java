package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet;

import net.minecraft.core.BlockPos;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.ExperienceOrb;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IContentsFilteredUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.XpHelper;

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
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}

		int cooldown = shouldPickupItems() ? pickupItems(entity, world, pos) : FULL_COOLDOWN_TICKS;

		if (shouldPickupXp() && canFillBackpackWithXp()) {
			cooldown = Math.min(cooldown, pickupXpOrbs(entity, world, pos));
		}

		setCooldown(world, cooldown);
	}

	private boolean canFillBackpackWithXp() {
		return backpackWrapper.getFluidHandler().map(fluidHandler -> fluidHandler.fill(new FluidStack(ModFluids.XP_STILL.get(), 1), IFluidHandler.FluidAction.SIMULATE) > 0).orElse(false);
	}

	private int pickupXpOrbs(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		List<ExperienceOrb> xpEntities = world.getEntities(EntityType.EXPERIENCE_ORB, new AABB(pos).inflate(upgradeItem.getRadius()), e -> true);
		if (xpEntities.isEmpty()) {
			return COOLDOWN_TICKS;
		}

		int cooldown = FULL_COOLDOWN_TICKS;
		for (ExperienceOrb xpOrb : xpEntities) {
			if (xpOrb.isAlive() && !canNotPickup(xpOrb, entity)) {
				if (tryToFillTank(xpOrb, world)) {
					cooldown = COOLDOWN_TICKS;
				} else {
					break;
				}
			}
		}
		return cooldown;
	}

	private boolean tryToFillTank(ExperienceOrb xpOrb, Level world) {
		int amountToTransfer = XpHelper.experienceToLiquid(xpOrb.value);

		return backpackWrapper.getFluidHandler().map(fluidHandler -> {
			int amountAdded = fluidHandler.fill(new FluidStack(ModFluids.XP_STILL.get(), amountToTransfer), IFluidHandler.FluidAction.EXECUTE);

			if (amountAdded > 0) {
				Vec3 pos = xpOrb.position();
				xpOrb.discard();

				if (amountToTransfer > amountAdded) {
					world.addFreshEntity(new ExperienceOrb(world, pos.x(), pos.y(), pos.z(), XpHelper.liquidToExperience(amountToTransfer - amountAdded)));
				}
				return true;
			}
			return false;
		}).orElse(false);
	}

	private int pickupItems(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		List<ItemEntity> itemEntities = world.getEntities(EntityType.ITEM, new AABB(pos).inflate(upgradeItem.getRadius()), e -> true);
		if (itemEntities.isEmpty()) {
			return COOLDOWN_TICKS;
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
		return cooldown;
	}

	private boolean isBlockedBySomething(Entity entity) {
		for (IMagnetPreventionChecker checker : magnetCheckers) {
			if (checker.isBlocked(entity)) {
				return true;
			}
		}
		return false;
	}

	private boolean canNotPickup(Entity entity, @Nullable LivingEntity player) {
		if (isBlockedBySomething(entity)) {
			return true;
		}

		CompoundTag data = entity.getPersistentData();
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

	public void setPickupItems(boolean pickupItems) {
		NBTHelper.setBoolean(upgrade, "pickupItems", pickupItems);
		save();
	}

	public boolean shouldPickupItems() {
		return NBTHelper.getBoolean(upgrade, "pickupItems").orElse(true);
	}

	public void setPickupXp(boolean pickupXp) {
		NBTHelper.setBoolean(upgrade, "pickupXp", pickupXp);
		save();
	}

	public boolean shouldPickupXp() {
		return NBTHelper.getBoolean(upgrade, "pickupXp").orElse(true);
	}
}
