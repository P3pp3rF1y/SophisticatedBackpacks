package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.block.entity.BlockEntity;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.functions.LootItemConditionalFunction;
import net.minecraft.world.level.storage.loot.functions.LootItemFunction;
import net.minecraft.world.level.storage.loot.functions.LootItemFunctionType;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModLoot;

public class CopyBackpackDataFunction extends LootItemConditionalFunction {
	protected CopyBackpackDataFunction(LootItemCondition[] conditionsIn) {
		super(conditionsIn);
	}

	@Override
	protected ItemStack run(ItemStack stack, LootContext context) {
		BlockEntity te = context.getParamOrNull(LootContextParams.BLOCK_ENTITY);
		if (te instanceof BackpackBlockEntity) {
			return ((BackpackBlockEntity) te).getBackpackWrapper().getBackpack();
		}

		return stack;
	}

	@Override
	public LootItemFunctionType getType() {
		return ModLoot.COPY_BACKPACK_DATA;
	}

	public static CopyBackpackDataFunction.Builder builder() {
		return new CopyBackpackDataFunction.Builder();
	}

	public static class Serializer extends LootItemConditionalFunction.Serializer<CopyBackpackDataFunction> {

		@Override
		public CopyBackpackDataFunction deserialize(JsonObject object, JsonDeserializationContext deserializationContext, LootItemCondition[] conditionsIn) {
			return new CopyBackpackDataFunction(conditionsIn);
		}
	}

	public static class Builder extends LootItemConditionalFunction.Builder<CopyBackpackDataFunction.Builder> {
		@Override
		protected CopyBackpackDataFunction.Builder getThis() {
			return this;
		}

		@Override
		public LootItemFunction build() {
			return new CopyBackpackDataFunction(getConditions());
		}
	}
}
