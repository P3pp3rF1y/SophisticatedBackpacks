package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import net.minecraft.block.Block;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.DirectoryCache;
import net.minecraft.data.IDataProvider;
import net.minecraft.loot.ConstantRange;
import net.minecraft.loot.ItemLootEntry;
import net.minecraft.loot.LootEntry;
import net.minecraft.loot.LootParameterSets;
import net.minecraft.loot.LootPool;
import net.minecraft.loot.LootTable;
import net.minecraft.loot.LootTableManager;
import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class BlockLootProvider implements IDataProvider {
	private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
	private final DataGenerator generator;

	BlockLootProvider(DataGenerator generator) {
		this.generator = generator;
	}

	@Override
	public void act(DirectoryCache cache) throws IOException {
		Map<Block, LootTable.Builder> tables = new HashMap<>();

		tables.put(ModBlocks.BACKPACK, getBackpack(ModItems.BACKPACK));
		tables.put(ModBlocks.IRON_BACKPACK, getBackpack(ModItems.IRON_BACKPACK));
		tables.put(ModBlocks.GOLD_BACKPACK, getBackpack(ModItems.GOLD_BACKPACK));
		tables.put(ModBlocks.DIAMOND_BACKPACK, getBackpack(ModItems.DIAMOND_BACKPACK));

		for (Map.Entry<Block, LootTable.Builder> e : tables.entrySet()) {
			Path path = getPath(generator.getOutputFolder(), e.getKey().getRegistryName());
			IDataProvider.save(GSON, cache, LootTableManager.toJson(e.getValue().setParameterSet(LootParameterSets.BLOCK).build()), path);
		}
	}

	@Override
	public String getName() {
		return "SophisticatedBackpacks block loot tables";
	}

	private static Path getPath(Path root, ResourceLocation id) {
		return root.resolve("data/" + id.getNamespace() + "/loot_tables/blocks/" + id.getPath() + ".json");
	}

	private static LootTable.Builder getBackpack(BackpackItem item) {
		LootEntry.Builder<?> entry = ItemLootEntry.builder(item);
		LootPool.Builder pool = LootPool.builder().name("main").rolls(ConstantRange.of(1)).addEntry(entry).acceptFunction(CopyBackpackDataFunction.builder());
		return LootTable.builder().addLootPool(pool);
	}
}
