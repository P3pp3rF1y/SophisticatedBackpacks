package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonObject;
import net.minecraft.item.ItemStack;
import net.minecraft.loot.LootContext;
import net.minecraft.loot.LootFunction;
import net.minecraft.loot.LootFunctionType;
import net.minecraft.loot.LootParameters;
import net.minecraft.loot.conditions.ILootCondition;
import net.minecraft.loot.functions.ILootFunction;
import net.minecraft.tileentity.TileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModLoot;

public class CopyBackpackDataFunction extends LootFunction {
	protected CopyBackpackDataFunction(ILootCondition[] conditionsIn) {
		super(conditionsIn);
	}

	@Override
	protected ItemStack run(ItemStack stack, LootContext context) {
		TileEntity te = context.getParamOrNull(LootParameters.BLOCK_ENTITY);
		if (te instanceof BackpackTileEntity) {
			return ((BackpackTileEntity) te).getBackpackWrapper().getBackpack();
		}

		return stack;
	}

	@Override
	public LootFunctionType getType() {
		return ModLoot.COPY_BACKPACK_DATA;
	}

	public static CopyBackpackDataFunction.Builder builder() {
		return new CopyBackpackDataFunction.Builder();
	}

	public static class Serializer extends LootFunction.Serializer<CopyBackpackDataFunction> {

		@Override
		public CopyBackpackDataFunction deserialize(JsonObject object, JsonDeserializationContext deserializationContext, ILootCondition[] conditionsIn) {
			return new CopyBackpackDataFunction(conditionsIn);
		}
	}

	public static class Builder extends LootFunction.Builder<CopyBackpackDataFunction.Builder> {
		@Override
		protected CopyBackpackDataFunction.Builder getThis() {
			return this;
		}

		@Override
		public ILootFunction build() {
			return new CopyBackpackDataFunction(getConditions());
		}
	}
}
