package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraftforge.common.ForgeConfigSpec;

public class FilteredUpgradeConfig extends FilteredUpgradeConfigBase {
	public FilteredUpgradeConfig(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow) {
		super(builder, name, path, defaultFilterSlots, defaultSlotsInRow);
		builder.pop();
	}
}
