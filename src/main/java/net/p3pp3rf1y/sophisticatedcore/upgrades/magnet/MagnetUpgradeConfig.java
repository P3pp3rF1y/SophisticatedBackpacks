package net.p3pp3rf1y.sophisticatedcore.upgrades.magnet;

import net.minecraftforge.common.ForgeConfigSpec;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilteredUpgradeConfigBase;

public class MagnetUpgradeConfig extends FilteredUpgradeConfigBase {
	public final ForgeConfigSpec.IntValue magnetRange;

	public MagnetUpgradeConfig(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow, int defaultMagnetRange) {
		super(builder, name, path, defaultFilterSlots, defaultSlotsInRow);
		magnetRange = builder.comment("Range around storage in blocks at which magnet will pickup items").defineInRange("magnetRange", defaultMagnetRange, 1, 20);
		builder.pop();
	}
}
