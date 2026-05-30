package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraftforge.common.ForgeConfigSpec;

import java.util.List;

public class MobCatcherUpgradeConfig {
	private static final String REGISTRY_NAME_MATCHER = "([a-z0-9_.-]+:[a-z0-9_/.-]+)";
	public final ForgeConfigSpec.IntValue basicMaxSlotCost;
	public final ForgeConfigSpec.IntValue advancedMaxSlotCost;
	public final ForgeConfigSpec.DoubleValue animalMultiplier;
	public final ForgeConfigSpec.DoubleValue hostileMultiplier;
	public final ForgeConfigSpec.BooleanValue disallowInventoryEntities;
	public final ForgeConfigSpec.ConfigValue<List<? extends String>> entityBlockList;
	public final ForgeConfigSpec.ConfigValue<List<? extends String>> hostileOverrides;
	public final ForgeConfigSpec.ConfigValue<List<? extends String>> passiveOverrides;

	public MobCatcherUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Mob Catcher Upgrade Settings").push("mobCatcherUpgrade");
		basicMaxSlotCost = builder.comment("Maximum slot cost a mob may have to be captured by the basic mob catcher. This is a hard limit, not a clamp.").defineInRange("basicMaxSlotCost", 18, 1, 120);
		advancedMaxSlotCost = builder.comment("Maximum slot cost a mob may have to be captured by the advanced mob catcher. This is a hard limit, not a clamp.").defineInRange("advancedMaxSlotCost", 72, 1, 120);
		animalMultiplier = builder.comment("Multiplier applied to animal mob slot cost.").defineInRange("animalMultiplier", 1D, 0.1D, 10D);
		hostileMultiplier = builder.comment("Multiplier applied to hostile mob slot cost.").defineInRange("hostileMultiplier", 2D, 0.1D, 10D);
		disallowInventoryEntities = builder.comment("When true, mobs that expose inventories or container menus are not capturable.").define("disallowInventoryEntities", false);
		entityBlockList = builder.comment("Entity types that cannot be captured.").defineListAllowEmpty("entityBlockList", List.of("minecraft:wither"), value -> value instanceof String s && s.matches(REGISTRY_NAME_MATCHER));
		hostileOverrides = builder.comment("Entity types treated as hostile by mob catcher slot cost and tier rules.").defineListAllowEmpty("hostileOverrides", List.of("minecraft:enderman"), value -> value instanceof String s && s.matches(REGISTRY_NAME_MATCHER));
		passiveOverrides = builder.comment("Entity types treated as passive by mob catcher slot cost and tier rules.").defineListAllowEmpty("passiveOverrides", List.of("minecraft:villager"), value -> value instanceof String s && s.matches(REGISTRY_NAME_MATCHER));
		builder.pop();
	}
}
