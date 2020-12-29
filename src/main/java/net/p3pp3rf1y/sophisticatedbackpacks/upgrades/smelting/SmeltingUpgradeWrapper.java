package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public class SmeltingUpgradeWrapper extends UpgradeWrapperBase<SmeltingUpgradeWrapper, SmeltingUpgradeItem> implements ITickableUpgrade, ISmeltingUpgrade {
	private static final int NOTHING_TO_DO_COOLDOWN = 10;

	private final SmeltingLogic smeltingLogic;

	public SmeltingUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
		smeltingLogic = new SmeltingLogic(upgrade, upgradeSaveHandler, Config.COMMON.smeltingUpgrade.smeltingSpeedMultiplier.get(),
				Config.COMMON.smeltingUpgrade.fuelEfficiencyMultiplier.get());
	}

	@Override
	public void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper) {
		if (isInCooldown(world)) {
			return;
		}

		if (!smeltingLogic.tick(world)) {
			setCooldown(world, NOTHING_TO_DO_COOLDOWN);
		}
	}

	@Override
	public SmeltingLogic getSmeltingLogic() {
		return smeltingLogic;
	}
}
