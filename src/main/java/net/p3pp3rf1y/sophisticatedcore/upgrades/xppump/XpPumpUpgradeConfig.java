package net.p3pp3rf1y.sophisticatedcore.upgrades.xppump;

import net.minecraftforge.common.ForgeConfigSpec;

public class XpPumpUpgradeConfig {
	public final ForgeConfigSpec.IntValue maxXpPointsPerMending;
	public final ForgeConfigSpec.BooleanValue mendingOn;

	public XpPumpUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Xp Pump Upgrade Settings").push("xpPumpUpgrade");
		mendingOn = builder.comment("Whether xp pump can mend items with mending. Set false here to turn off the feature altogether.").define("mendingOn", true);
		maxXpPointsPerMending = builder.comment("How many experience points at a maximum would be used to mend an item per operation (every 5 ticks and 1 xp point usually translates to 2 damage repaired).").defineInRange("maxXpPointsPerMending", 5, 1, 20);
		builder.pop();
	}
}
