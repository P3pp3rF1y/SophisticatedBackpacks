package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.xppump;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.material.Fluid;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackFluidHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.XpHelper;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class XpPumpUpgradeWrapper extends UpgradeWrapperBase<XpPumpUpgradeWrapper, XpPumpUpgradeItem> implements ITickableUpgrade {
	private static final int DEFAULT_LEVEL = 10;
	private static final int COOLDOWN = 5;
	private static final int ALL_LEVELS = 10000;
	private static final int PLAYER_SEARCH_RANGE = 3;

	protected XpPumpUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if ((entity != null && !(entity instanceof Player)) || isInCooldown(world)) {
			return;
		}

		if (entity == null) {
			AABB searchBox = new AABB(pos).inflate(PLAYER_SEARCH_RANGE);
			for (Player player : world.players()) {
				if (searchBox.contains(player.getX(), player.getY(), player.getZ())) {
					interactWithPlayer(player);
				}
			}
		} else {
			Player player = (Player) entity;
			interactWithPlayer(player);
		}

		setCooldown(world, COOLDOWN);
	}

	private void interactWithPlayer(Player player) {
		backpackWrapper.getFluidHandler().ifPresent(fluidHandler -> {
			int level = getLevel();
			AutomationDirection direction = getDirection();
			if (direction == AutomationDirection.OFF) {
				return;
			}

			if (direction == AutomationDirection.INPUT) {
				if (level < player.experienceLevel || (level == player.experienceLevel && player.experienceProgress > 0)) {
					tryFillTankWithPlayerExperience(player, fluidHandler, level, false);
				}
			} else if (direction == AutomationDirection.OUTPUT && level > player.experienceLevel) {
				tryGivePlayerExperienceFromTank(player, fluidHandler, level, false);
			}
		});
	}

	private void tryGivePlayerExperienceFromTank(Player player, IBackpackFluidHandler fluidHandler, int stopAtLevel) {
		tryGivePlayerExperienceFromTank(player, fluidHandler, stopAtLevel, true);
	}

	private void tryGivePlayerExperienceFromTank(Player player, IBackpackFluidHandler fluidHandler, int stopAtLevel, boolean ignoreInOutLimit) {
		int maxXpPointsToGive = XpHelper.getExperienceForLevel(stopAtLevel) - XpHelper.getPlayerTotalExperience(player);
		FluidStack toDrain = new FluidStack(getExperienceFluidFromHandlerOrDefault(fluidHandler), XpHelper.experienceToLiquid(maxXpPointsToGive));

		FluidStack drained = fluidHandler.drain(toDrain, IFluidHandler.FluidAction.EXECUTE, ignoreInOutLimit);

		if (!drained.isEmpty()) {
			player.giveExperiencePoints(XpHelper.liquidToExperience(drained.getAmount()));
		}
	}

	private Fluid getExperienceFluidFromHandlerOrDefault(IFluidHandler fluidHandler) {
		for (int tank = 0; tank < fluidHandler.getTanks(); tank++) {
			Fluid fluidInTank = fluidHandler.getFluidInTank(tank).getFluid();
			if (fluidInTank.is(ModFluids.EXPERIENCE_TAG)) {
				return fluidInTank;
			}
		}
		return ModFluids.XP_STILL.get();
	}

	private void tryFillTankWithPlayerExperience(Player player, IBackpackFluidHandler fluidHandler, int stopAtLevel) {
		tryFillTankWithPlayerExperience(player, fluidHandler, stopAtLevel, true);
	}

	private void tryFillTankWithPlayerExperience(Player player, IBackpackFluidHandler fluidHandler, int stopAtLevel, boolean ignoreInOutLimit) {
		int maxXpPointsToTake = XpHelper.getPlayerTotalExperience(player) - XpHelper.getExperienceForLevel(stopAtLevel);
		FluidStack toFill = new FluidStack(getExperienceFluidFromHandlerOrDefault(fluidHandler), XpHelper.experienceToLiquid(maxXpPointsToTake));
		int filled = fluidHandler.fill(toFill, IFluidHandler.FluidAction.EXECUTE, ignoreInOutLimit);

		if (filled > 0) {
			player.giveExperiencePoints(-XpHelper.liquidToExperience(filled));
		}
	}

	public void takeLevelsFromPlayer(Player player) {
		backpackWrapper.getFluidHandler().ifPresent(fluidHandler -> tryFillTankWithPlayerExperience(player, fluidHandler, Math.max(player.experienceLevel - getLevelsToStore(), 0)));
	}

	public void takeAllExperienceFromPlayer(Player player) {
		backpackWrapper.getFluidHandler().ifPresent(fluidHandler -> tryFillTankWithPlayerExperience(player, fluidHandler, 0));
	}

	public void giveLevelsToPlayer(Player player) {
		backpackWrapper.getFluidHandler().ifPresent(fluidHandler -> tryGivePlayerExperienceFromTank(player, fluidHandler, player.experienceLevel + getLevelsToTake()));
	}

	public void giveAllExperienceToPlayer(Player player) {
		backpackWrapper.getFluidHandler().ifPresent(fluidHandler -> tryGivePlayerExperienceFromTank(player, fluidHandler, ALL_LEVELS));
	}

	public AutomationDirection getDirection() {
		return NBTHelper.getEnumConstant(upgrade, "direction", AutomationDirection::fromName).orElse(AutomationDirection.INPUT);
	}

	public void setDirection(AutomationDirection direction) {
		NBTHelper.setEnumConstant(upgrade, "direction", direction);
		save();
	}

	public void setLevel(int level) {
		NBTHelper.setInteger(upgrade, "level", level);
		save();
	}

	public int getLevel() {
		return NBTHelper.getInt(upgrade, "level").orElse(DEFAULT_LEVEL);
	}

	public void setLevelsToStore(int levelsToTake) {
		NBTHelper.setInteger(upgrade, "levelsToStore", levelsToTake);
		save();
	}

	public int getLevelsToStore() {
		return NBTHelper.getInt(upgrade, "levelsToStore").orElse(1);
	}

	public void setLevelsToTake(int levelsToGive) {
		NBTHelper.setInteger(upgrade, "levelsToTake", levelsToGive);
		save();
	}

	public int getLevelsToTake() {
		return NBTHelper.getInt(upgrade, "levelsToTake").orElse(1);
	}
}
