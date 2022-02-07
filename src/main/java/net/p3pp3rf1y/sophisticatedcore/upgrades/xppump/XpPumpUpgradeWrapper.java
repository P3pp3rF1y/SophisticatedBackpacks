package net.p3pp3rf1y.sophisticatedcore.upgrades.xppump;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.AABB;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageFluidHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModFluids;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedcore.util.XpHelper;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.function.Consumer;

public class XpPumpUpgradeWrapper extends UpgradeWrapperBase<XpPumpUpgradeWrapper, XpPumpUpgradeItem> implements ITickableUpgrade {
	private static final int DEFAULT_LEVEL = 10;
	private static final int COOLDOWN = 5;
	private static final int ALL_LEVELS = 10000;
	private static final int PLAYER_SEARCH_RANGE = 3;

	private final XpPumpUpgradeConfig xpPumpUpgradeConfig;

	protected XpPumpUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
		xpPumpUpgradeConfig = upgradeItem.getXpPumpUpgradeConfig();
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
					mendItems(player);
				}
			}
		} else {
			Player player = (Player) entity;
			interactWithPlayer(player);
			mendItems(player);
		}

		setCooldown(world, COOLDOWN);
	}

	private void mendItems(Player player) {
		if (Boolean.FALSE.equals(xpPumpUpgradeConfig.mendingOn.get()) || !shouldMendItems()) {
			return;
		}

		Map.Entry<EquipmentSlot, ItemStack> entry = EnchantmentHelper.getRandomItemWith(Enchantments.MENDING, player, ItemStack::isDamaged);
		if (entry != null) {
			ItemStack itemStack = entry.getValue();
			if (!itemStack.isEmpty() && itemStack.isDamaged() && itemStack.getXpRepairRatio() > 0) {
				float xpToTryDrain = Math.min(xpPumpUpgradeConfig.maxXpPointsPerMending.get(), itemStack.getDamageValue() / itemStack.getXpRepairRatio());
				if (xpToTryDrain > 0) {
					storageWrapper.getFluidHandler().ifPresent(fluidHandler -> {
						FluidStack drained = fluidHandler.drain(ModFluids.EXPERIENCE_TAG, XpHelper.experienceToLiquid(xpToTryDrain), IFluidHandler.FluidAction.EXECUTE, false);
						float xpDrained = XpHelper.liquidToExperience(drained.getAmount());
						int durationToRepair = (int) (xpDrained * itemStack.getXpRepairRatio());
						itemStack.setDamageValue(itemStack.getDamageValue() - durationToRepair);
					});
				}
			}
		}
	}

	private void interactWithPlayer(Player player) {
		storageWrapper.getFluidHandler().ifPresent(fluidHandler -> {
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

	private void tryGivePlayerExperienceFromTank(Player player, IStorageFluidHandler fluidHandler, int stopAtLevel) {
		tryGivePlayerExperienceFromTank(player, fluidHandler, stopAtLevel, true);
	}

	private void tryGivePlayerExperienceFromTank(Player player, IStorageFluidHandler fluidHandler, int stopAtLevel, boolean ignoreInOutLimit) {
		int maxXpPointsToGive = XpHelper.getExperienceForLevel(stopAtLevel) - XpHelper.getPlayerTotalExperience(player);
		FluidStack drained = fluidHandler.drain(ModFluids.EXPERIENCE_TAG, XpHelper.experienceToLiquid(maxXpPointsToGive), IFluidHandler.FluidAction.EXECUTE, ignoreInOutLimit);

		if (!drained.isEmpty()) {
			player.giveExperiencePoints((int) XpHelper.liquidToExperience(drained.getAmount()));
		}
	}

	private void tryFillTankWithPlayerExperience(Player player, IStorageFluidHandler fluidHandler, int stopAtLevel) {
		tryFillTankWithPlayerExperience(player, fluidHandler, stopAtLevel, true);
	}

	private void tryFillTankWithPlayerExperience(Player player, IStorageFluidHandler fluidHandler, int stopAtLevel, boolean ignoreInOutLimit) {
		int maxXpPointsToTake = XpHelper.getPlayerTotalExperience(player) - XpHelper.getExperienceForLevel(stopAtLevel);
		int filled = fluidHandler.fill(ModFluids.EXPERIENCE_TAG, XpHelper.experienceToLiquid(maxXpPointsToTake), ModFluids.XP_STILL.get(), IFluidHandler.FluidAction.EXECUTE, ignoreInOutLimit);

		if (filled > 0) {
			player.giveExperiencePoints((int) -XpHelper.liquidToExperience(filled));
		}
	}

	public void takeLevelsFromPlayer(Player player) {
		storageWrapper.getFluidHandler().ifPresent(fluidHandler -> tryFillTankWithPlayerExperience(player, fluidHandler, Math.max(player.experienceLevel - getLevelsToStore(), 0)));
	}

	public void takeAllExperienceFromPlayer(Player player) {
		storageWrapper.getFluidHandler().ifPresent(fluidHandler -> tryFillTankWithPlayerExperience(player, fluidHandler, 0));
	}

	public void giveLevelsToPlayer(Player player) {
		storageWrapper.getFluidHandler().ifPresent(fluidHandler -> tryGivePlayerExperienceFromTank(player, fluidHandler, player.experienceLevel + getLevelsToTake()));
	}

	public void giveAllExperienceToPlayer(Player player) {
		storageWrapper.getFluidHandler().ifPresent(fluidHandler -> tryGivePlayerExperienceFromTank(player, fluidHandler, ALL_LEVELS));
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

	public boolean shouldMendItems() {
		return NBTHelper.getBoolean(upgrade, "mendItems").orElse(true);
	}

	public void setMendItems(boolean mendItems) {
		NBTHelper.setBoolean(upgrade, "mendItems", mendItems);
		save();
	}
}
