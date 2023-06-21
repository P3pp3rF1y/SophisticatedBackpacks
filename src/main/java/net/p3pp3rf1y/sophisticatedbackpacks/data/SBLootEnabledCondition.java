package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializationContext;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.minecraft.world.level.storage.loot.predicates.LootItemConditionType;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

public class SBLootEnabledCondition implements LootItemCondition {

	private SBLootEnabledCondition() {
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
			return new SBLootEnabledCondition();
		}
	}

	public static class Serializer implements net.minecraft.world.level.storage.loot.Serializer<SBLootEnabledCondition> {
		@Override
		public void serialize(JsonObject object, SBLootEnabledCondition instance, JsonSerializationContext ctx) {
			//nothing to serialize
		}

		@Override
		public SBLootEnabledCondition deserialize(JsonObject object, JsonDeserializationContext ctx) {
			return new SBLootEnabledCondition();
		}
	}
}
