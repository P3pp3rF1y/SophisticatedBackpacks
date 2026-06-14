package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.TagKey;
import net.minecraft.world.entity.EntityType;
import net.neoforged.neoforge.common.ModConfigSpec;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.List;

public class MobCatcherUpgradeConfig {
	private static final String ENTITY_MATCHER = "#?([a-z0-9_.-]+:[a-z0-9_/.-]+)";
	public final ModConfigSpec.IntValue basicMaxSlotCost;
	public final ModConfigSpec.IntValue advancedMaxSlotCost;
	public final ModConfigSpec.DoubleValue animalMultiplier;
	public final ModConfigSpec.DoubleValue hostileMultiplier;
	public final ModConfigSpec.BooleanValue disallowInventoryEntities;
	public final ModConfigSpec.ConfigValue<List<? extends String>> entityBlockList;
	public final ModConfigSpec.ConfigValue<List<? extends String>> hostileOverrides;
	public final ModConfigSpec.ConfigValue<List<? extends String>> passiveOverrides;
	@Nullable
	private List<EntityMatcher> entityBlockListMatchers = null;
	@Nullable
	private List<EntityMatcher> hostileOverrideMatchers = null;
	@Nullable
	private List<EntityMatcher> passiveOverrideMatchers = null;

	public MobCatcherUpgradeConfig(ModConfigSpec.Builder builder) {
		builder.comment("Mob Catcher Upgrade Settings").push("mobCatcherUpgrade");
		basicMaxSlotCost = builder.comment("Maximum slot cost a mob may have to be captured by the basic mob catcher. This is a hard limit, not a clamp.").defineInRange("basicMaxSlotCost", 18, 1, 120);
		advancedMaxSlotCost = builder.comment("Maximum slot cost a mob may have to be captured by the advanced mob catcher. This is a hard limit, not a clamp.").defineInRange("advancedMaxSlotCost", 72, 1, 120);
		animalMultiplier = builder.comment("Multiplier applied to animal mob slot cost.").defineInRange("animalMultiplier", 1D, 0.1D, 10D);
		hostileMultiplier = builder.comment("Multiplier applied to hostile mob slot cost.").defineInRange("hostileMultiplier", 2D, 0.1D, 10D);
		disallowInventoryEntities = builder.comment("When true, mobs that expose inventories or container menus are not capturable.").define("disallowInventoryEntities", false);
		entityBlockList = builder.comment("Entity types or entity tags prefixed with # that cannot be captured.").defineListAllowEmpty("entityBlockList", List::of, () -> "minecraft:wither", value -> value instanceof String s && s.matches(ENTITY_MATCHER));
		hostileOverrides = builder.comment("Entity types or entity tags prefixed with # treated as hostile by mob catcher slot cost and tier rules.").defineListAllowEmpty("hostileOverrides", List::of, () -> "minecraft:enderman", value -> value instanceof String s && s.matches(ENTITY_MATCHER));
		passiveOverrides = builder.comment("Entity types or entity tags prefixed with # treated as passive by mob catcher slot cost and tier rules.").defineListAllowEmpty("passiveOverrides", List::of, () -> "minecraft:villager", value -> value instanceof String s && s.matches(ENTITY_MATCHER));
		builder.pop();
	}

	public boolean matchesEntityBlockList(EntityType<?> entityType) {
		if (entityBlockListMatchers == null) {
			entityBlockListMatchers = getEntityMatchers(entityBlockList.get());
		}
		return matches(entityBlockListMatchers, entityType);
	}

	public boolean matchesHostileOverrides(EntityType<?> entityType) {
		if (hostileOverrideMatchers == null) {
			hostileOverrideMatchers = getEntityMatchers(hostileOverrides.get());
		}
		return matches(hostileOverrideMatchers, entityType);
	}

	public boolean matchesPassiveOverrides(EntityType<?> entityType) {
		if (passiveOverrideMatchers == null) {
			passiveOverrideMatchers = getEntityMatchers(passiveOverrides.get());
		}
		return matches(passiveOverrideMatchers, entityType);
	}

	public void clearCache() {
		entityBlockListMatchers = null;
		hostileOverrideMatchers = null;
		passiveOverrideMatchers = null;
	}

	private static List<EntityMatcher> getEntityMatchers(List<? extends String> configuredEntities) {
		List<EntityMatcher> matchers = new ArrayList<>();
		for (String configuredEntity : configuredEntities) {
			matchers.add(getEntityMatcher(configuredEntity));
		}
		return matchers;
	}

	private static EntityMatcher getEntityMatcher(String configuredEntity) {
		if (configuredEntity.startsWith("#")) {
			return new EntityTagMatcher(TagKey.create(Registries.ENTITY_TYPE, ResourceLocation.parse(configuredEntity.substring(1))));
		}
		return new EntityTypeMatcher(ResourceLocation.parse(configuredEntity));
	}

	private static boolean matches(List<EntityMatcher> matchers, EntityType<?> entityType) {
		ResourceLocation registryName = BuiltInRegistries.ENTITY_TYPE.getKey(entityType);
		for (EntityMatcher matcher : matchers) {
			if (matcher.matches(entityType, registryName)) {
				return true;
			}
		}
		return false;
	}

	private interface EntityMatcher {
		boolean matches(EntityType<?> entityType, ResourceLocation registryName);
	}

	private record EntityTypeMatcher(ResourceLocation matchedEntityType) implements EntityMatcher {
		@Override
		public boolean matches(EntityType<?> entityType, ResourceLocation registryName) {
			return matchedEntityType.equals(registryName);
		}
	}

	private record EntityTagMatcher(TagKey<EntityType<?>> tag) implements EntityMatcher {
		@Override
		public boolean matches(EntityType<?> entityType, ResourceLocation registryName) {
			return entityType.is(tag);
		}
	}
}
