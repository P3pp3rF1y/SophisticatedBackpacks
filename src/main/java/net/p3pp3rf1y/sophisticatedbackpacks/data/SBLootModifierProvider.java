package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import it.unimi.dsi.fastutil.objects.ObjectArrayList;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.storage.loot.BuiltInLootTables;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.minecraftforge.common.data.GlobalLootModifierProvider;
import net.minecraftforge.common.loot.IGlobalLootModifier;
import net.minecraftforge.common.loot.LootModifier;
import net.minecraftforge.common.loot.LootTableIdCondition;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

public class SBLootModifierProvider extends GlobalLootModifierProvider {

	SBLootModifierProvider(PackOutput packOutput) {
		super(packOutput, SophisticatedBackpacks.MOD_ID);
	}

	@Override
	protected void start() {
		addInjectLootTableModifier(SBInjectLootSubProvider.SIMPLE_DUNGEON, BuiltInLootTables.SIMPLE_DUNGEON);
		addInjectLootTableModifier(SBInjectLootSubProvider.ABANDONED_MINESHAFT, BuiltInLootTables.ABANDONED_MINESHAFT);
		addInjectLootTableModifier(SBInjectLootSubProvider.DESERT_PYRAMID, BuiltInLootTables.DESERT_PYRAMID);
		addInjectLootTableModifier(SBInjectLootSubProvider.WOODLAND_MANSION, BuiltInLootTables.WOODLAND_MANSION);
		addInjectLootTableModifier(SBInjectLootSubProvider.SHIPWRECK_TREASURE, BuiltInLootTables.SHIPWRECK_TREASURE);
		addInjectLootTableModifier(SBInjectLootSubProvider.BASTION_TREASURE, BuiltInLootTables.BASTION_TREASURE);
		addInjectLootTableModifier(SBInjectLootSubProvider.END_CITY_TREASURE, BuiltInLootTables.END_CITY_TREASURE);
		addInjectLootTableModifier(SBInjectLootSubProvider.NETHER_BRIDGE, BuiltInLootTables.NETHER_BRIDGE);
	}

	private void addInjectLootTableModifier(ResourceLocation lootTable, ResourceLocation lootTableToInjectInto) {
		add(lootTableToInjectInto.getPath(), new InjectLootModifier(lootTable, lootTableToInjectInto));
	}

	public static class InjectLootModifier extends LootModifier {
		public static final Codec<InjectLootModifier> CODEC = RecordCodecBuilder.create(inst -> LootModifier.codecStart(inst).and(
				inst.group(
						ResourceLocation.CODEC.fieldOf("loot_table").forGetter(m -> m.lootTable),
						ResourceLocation.CODEC.fieldOf("loot_table_to_inject_into").forGetter(m -> m.lootTableToInjectInto)
				)
		).apply(inst, InjectLootModifier::new));
		private final ResourceLocation lootTable;
		private final ResourceLocation lootTableToInjectInto;

		protected InjectLootModifier(LootItemCondition[] conditions, ResourceLocation lootTable, ResourceLocation lootTableToInjectInto) {
			super(conditions);
			this.lootTable = lootTable;
			this.lootTableToInjectInto = lootTableToInjectInto;
		}

		protected InjectLootModifier(ResourceLocation lootTable, ResourceLocation lootTableToInjectInto) {
			this(new LootItemCondition[] {SBLootEnabledCondition.builder().build(),
					LootTableIdCondition.builder(lootTableToInjectInto).build()}, lootTable, lootTableToInjectInto);
		}

		@Override
		protected ObjectArrayList<ItemStack> doApply(ObjectArrayList<ItemStack> generatedLoot, LootContext context) {
			LootTable table = context.getResolver().getLootTable(lootTable);
			table.getRandomItemsRaw(context, generatedLoot::add);
			return generatedLoot;
		}

		@Override
		public Codec<? extends IGlobalLootModifier> codec() {
			return ModItems.INJECT_LOOT.get();
		}
	}
}
