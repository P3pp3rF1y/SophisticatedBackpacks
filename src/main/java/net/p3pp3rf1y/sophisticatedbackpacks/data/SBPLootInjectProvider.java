package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.DataProvider;
import net.minecraft.data.HashCache;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.storage.loot.BuiltInLootTables;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.LootTables;
import net.minecraft.world.level.storage.loot.entries.EmptyLootItem;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.providers.number.ConstantValue;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class SBPLootInjectProvider implements DataProvider {
	private static final Gson GSON = new GsonBuilder().setPrettyPrinting().create();
	private final DataGenerator generator;

	SBPLootInjectProvider(DataGenerator generator) {
		this.generator = generator;
	}

	@Override
	public void run(HashCache cache) throws IOException {
		Map<ResourceLocation, LootTable.Builder> tables = new HashMap<>();

		tables.put(BuiltInLootTables.SIMPLE_DUNGEON, getLootTable(90,
				getItemLootEntry(ModItems.BACKPACK.get(), 5),
				getItemLootEntry(ModItems.COPPER_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.PICKUP_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.ABANDONED_MINESHAFT, getLootTable(84,
				getItemLootEntry(ModItems.BACKPACK.get(), 7),
				getItemLootEntry(ModItems.COPPER_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 1),
				getItemLootEntry(ModItems.MAGNET_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.DESERT_PYRAMID, getLootTable(89,
				getItemLootEntry(ModItems.COPPER_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 1),
				getItemLootEntry(ModItems.MAGNET_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.SHIPWRECK_TREASURE, getLootTable(92,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 4),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 2),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.WOODLAND_MANSION, getLootTable(92,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 4),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 2),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.NETHER_BRIDGE, getLootTable(90,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.FEEDING_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.BASTION_TREASURE, getLootTable(90,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.FEEDING_UPGRADE.get(), 2)));
		tables.put(BuiltInLootTables.END_CITY_TREASURE, getLootTable(90,
				getItemLootEntry(ModItems.DIAMOND_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));

		for (Map.Entry<ResourceLocation, LootTable.Builder> e : tables.entrySet()) {
			Path path = getPath(generator.getOutputFolder(), e.getKey());
			DataProvider.save(GSON, cache, LootTables.serialize(e.getValue().setParamSet(LootContextParamSets.CHEST).build()), path);
		}
	}

	@Override
	public String getName() {
		return "SophisticatedBackpacks chest loot additions";
	}

	private static Path getPath(Path root, ResourceLocation id) {
		return root.resolve("data/" + SophisticatedBackpacks.MOD_ID + "/loot_tables/inject/" + id.getPath() + ".json");
	}

	private LootPoolEntryContainer.Builder<?> getItemLootEntry(Item item, int weight) {
		return LootItem.lootTableItem(item).setWeight(weight);
	}

	private static LootTable.Builder getLootTable(int emptyWeight, LootPoolEntryContainer.Builder<?>... entries) {
		LootPool.Builder pool = LootPool.lootPool().name("main").setRolls(ConstantValue.exactly(1));
		for (LootPoolEntryContainer.Builder<?> entry : entries) {
			pool.add(entry);
		}
		pool.add(EmptyLootItem.emptyItem().setWeight(emptyWeight));
		return LootTable.lootTable().withPool(pool);
	}
}
