package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.loot.BlockLootSubProvider;
import net.minecraft.world.flag.FeatureFlags;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.providers.number.ConstantValue;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Map;
import java.util.Set;

public class SBPBlockLootSubProvider extends BlockLootSubProvider {
	protected SBPBlockLootSubProvider() {
		super(Set.of(), FeatureFlags.REGISTRY.allFlags());
	}

	@Override
	public void generate() {
		add(ModBlocks.BACKPACK.get(), dropBackpackWithContents(ModItems.BACKPACK.get()));
		add(ModBlocks.IRON_BACKPACK.get(), dropBackpackWithContents(ModItems.IRON_BACKPACK.get()));
		add(ModBlocks.GOLD_BACKPACK.get(), dropBackpackWithContents(ModItems.GOLD_BACKPACK.get()));
		add(ModBlocks.DIAMOND_BACKPACK.get(), dropBackpackWithContents(ModItems.DIAMOND_BACKPACK.get()));
		add(ModBlocks.NETHERITE_BACKPACK.get(), dropBackpackWithContents(ModItems.NETHERITE_BACKPACK.get()));
	}

	@Override
	protected Iterable<Block> getKnownBlocks() {
		return ForgeRegistries.BLOCKS.getEntries().stream()
				.filter(e -> e.getKey().location().getNamespace().equals(SophisticatedBackpacks.MOD_ID))
				.map(Map.Entry::getValue)
				.toList();
	}

	private static LootTable.Builder dropBackpackWithContents(BackpackItem item) {
		LootPoolEntryContainer.Builder<?> entry = LootItem.lootTableItem(item);
		LootPool.Builder pool = LootPool.lootPool().name("main").setRolls(ConstantValue.exactly(1)).add(entry).apply(CopyBackpackDataFunction.builder());
		return LootTable.lootTable().withPool(pool);
	}
}
