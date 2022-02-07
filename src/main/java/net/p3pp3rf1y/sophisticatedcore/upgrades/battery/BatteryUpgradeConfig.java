package net.p3pp3rf1y.sophisticatedcore.upgrades.battery;

import net.minecraftforge.common.ForgeConfigSpec;

public class BatteryUpgradeConfig {
	public final ForgeConfigSpec.IntValue energyPerSlotRow;
	public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
	public final ForgeConfigSpec.IntValue maxInputOutput;

	public BatteryUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Tank Upgrade Settings").push("tankUpgrade");
		energyPerSlotRow = builder.comment("Energy in FE the battery upgrade will have per row of storage slots").defineInRange("energyPerSlotRow", 10000, 500, 50000);
		stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to max energy of the battery and max in/out. Value lower than 1 makes stack multiplier affect the max energy less, higher makes it affect the max energy more. 0 turns off stack multiplier affecting battery upgrade").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
		maxInputOutput = builder.comment("How much FE can be transfered in / out per operation. This is a base transfer rate and same as max capacity gets multiplied by number of rows in storage and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
		builder.pop();
	}
}
