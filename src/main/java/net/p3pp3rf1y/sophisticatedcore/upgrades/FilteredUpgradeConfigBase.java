package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraftforge.common.ForgeConfigSpec;

public class FilteredUpgradeConfigBase {
	public final ForgeConfigSpec.IntValue filterSlots;
	public final ForgeConfigSpec.IntValue slotsInRow;

	protected FilteredUpgradeConfigBase(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow) {
		builder.comment(name + " Settings").push(path);
		filterSlots = builder.comment("Number of " + name + "'s filter slots").defineInRange("filterSlots", defaultFilterSlots, 1, 20);
		slotsInRow = builder.comment("Number of filter slots displayed in a row").defineInRange("slotsInRow", defaultSlotsInRow, 1, 6);
	}
}
