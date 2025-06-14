package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.mojang.serialization.MapCodec;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.minecraft.world.level.storage.loot.predicates.LootItemConditionType;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

public class BackpackLootEnabledCondition implements LootItemCondition {

	private static final BackpackLootEnabledCondition INSTANCE = new BackpackLootEnabledCondition();
	public static final MapCodec<BackpackLootEnabledCondition> CODEC = MapCodec.unit(INSTANCE).stable();
	private BackpackLootEnabledCondition() {
	}

	@Override
	public LootItemConditionType getType() {
		return ModItems.LOOT_ENABLED_CONDITION.get();
	}

	@Override
	public boolean test(LootContext lootContext) {
		return Boolean.TRUE.equals(Config.COMMON.chestLootEnabled.get());
	}

	public static Builder builder() {
		return new Builder();
	}

	public static class Builder implements LootItemCondition.Builder {
		@Override
		public LootItemCondition build() {
			return new BackpackLootEnabledCondition();
		}
	}
}
