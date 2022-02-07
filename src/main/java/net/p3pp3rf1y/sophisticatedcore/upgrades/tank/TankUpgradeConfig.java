package net.p3pp3rf1y.sophisticatedcore.upgrades.tank;

import net.minecraftforge.common.ForgeConfigSpec;

public class TankUpgradeConfig {
	public final ForgeConfigSpec.IntValue capacityPerSlotRow;
	public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
	public final ForgeConfigSpec.IntValue autoFillDrainContainerCooldown;
	public final ForgeConfigSpec.IntValue maxInputOutput;

	public TankUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Tank Upgrade Settings").push("tankUpgrade");
		capacityPerSlotRow = builder.comment("Capacity in mB the tank upgrade will have per row of storage slots").defineInRange("capacityPerSlotRow", 4000, 500, 20000);
		stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to tank capacity. Value lower than 1 makes stack multiplier affect the capacity less, higher makes it affect the capacity more. 0 turns off stack multiplier affecting tank capacity").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
		autoFillDrainContainerCooldown = builder.comment("Cooldown between fill/drain actions done on fluid containers in tank slots. Only fills/drains one bucket worth to/from container after this cooldown and then waits again.").defineInRange("autoFillDrainContainerCooldown", 20, 1, 100);
		maxInputOutput = builder.comment("How much mB can be transfered in / out per operation. This is a base transfer rate and same as max tank capacity gets multiplied by number of rows in storage and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
		builder.pop();
	}
}
