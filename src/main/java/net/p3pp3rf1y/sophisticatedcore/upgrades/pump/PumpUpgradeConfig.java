package net.p3pp3rf1y.sophisticatedcore.upgrades.pump;

import net.minecraftforge.common.ForgeConfigSpec;

public class PumpUpgradeConfig {
	public final ForgeConfigSpec.IntValue maxInputOutput;
	public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
	public final ForgeConfigSpec.IntValue filterSlots;

	public PumpUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Pump Upgrade Settings").push("pumpUpgrade");
		filterSlots = builder.comment("Number of fluid filter slots").defineInRange("filterSlots", 4, 1, 20);
		maxInputOutput = builder.comment("How much mB can be transfered in / out per operation. This is a base transfer rate that gets multiplied by number of rows in storage and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
		stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to max input/output value. Value lower than 1 makes stack multiplier affect the capacity less, higher makes it affect the capacity more. 0 turns off stack multiplier affecting input/output").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
		builder.pop();
	}
}
