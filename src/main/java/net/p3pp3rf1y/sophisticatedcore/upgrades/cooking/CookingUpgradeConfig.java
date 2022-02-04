package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraftforge.common.ForgeConfigSpec;

public class CookingUpgradeConfig {
	public final ForgeConfigSpec.DoubleValue cookingSpeedMultiplier;
	public final ForgeConfigSpec.DoubleValue fuelEfficiencyMultiplier;

	public CookingUpgradeConfig(ForgeConfigSpec.Builder builder, final String upgradeName, String path) {
		builder.comment(upgradeName + " Settings").push(path);
		cookingSpeedMultiplier = builder.comment("Smelting speed multiplier (1.0 equals speed at which vanilla furnace smelts items)")
				.defineInRange("smeltingSpeedMultiplier", 1.0D, 0.25D, 4.0D);
		fuelEfficiencyMultiplier = builder.comment("Fuel efficiency multiplier (1.0 equals speed at which it's used in vanilla furnace)")
				.defineInRange("fuelEfficiencyMultiplier", 1.0D, 0.25D, 4.0D);
	}
}
