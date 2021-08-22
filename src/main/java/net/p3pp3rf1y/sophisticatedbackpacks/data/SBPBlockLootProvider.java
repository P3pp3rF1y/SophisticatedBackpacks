package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
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
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class SBPBlockLootProvider implements IDataProvider {
	private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
	private final DataGenerator generator;

	SBPBlockLootProvider(DataGenerator generator) {
		this.generator = generator;
	}

	@Override
	public void run(DirectoryCache cache) throws IOException {
		Map<ResourceLocation, LootTable.Builder> tables = new HashMap<>();

		tables.put(ModBlocks.BACKPACK.getId(), getBackpack(ModItems.BACKPACK.get()));
		tables.put(ModBlocks.IRON_BACKPACK.getId(), getBackpack(ModItems.IRON_BACKPACK.get()));
		tables.put(ModBlocks.GOLD_BACKPACK.getId(), getBackpack(ModItems.GOLD_BACKPACK.get()));
		tables.put(ModBlocks.DIAMOND_BACKPACK.getId(), getBackpack(ModItems.DIAMOND_BACKPACK.get()));
		tables.put(ModBlocks.NETHERITE_BACKPACK.getId(), getBackpack(ModItems.NETHERITE_BACKPACK.get()));

		for (Map.Entry<ResourceLocation, LootTable.Builder> e : tables.entrySet()) {
			Path path = getPath(generator.getOutputFolder(), e.getKey());
			IDataProvider.save(GSON, cache, LootTableManager.serialize(e.getValue().setParamSet(LootParameterSets.BLOCK).build()), path);
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
		LootEntry.Builder<?> entry = ItemLootEntry.lootTableItem(item);
		LootPool.Builder pool = LootPool.lootPool().name("main").setRolls(ConstantRange.exactly(1)).add(entry).apply(CopyBackpackDataFunction.builder());
		return LootTable.lootTable().withPool(pool);
	}
}
