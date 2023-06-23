package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.loot.LootTableSubProvider;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.storage.loot.BuiltInLootTables;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.entries.EmptyLootItem;
import net.minecraft.world.level.storage.loot.entries.LootItem;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.providers.number.ConstantValue;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.Set;
import java.util.function.BiConsumer;

public class SBInjectLootSubProvider implements LootTableSubProvider {
	private static final String INJECT_FOLDER = "inject/";
	public static final ResourceLocation ABANDONED_MINESHAFT = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.ABANDONED_MINESHAFT.getPath());
	public static final ResourceLocation BASTION_TREASURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.BASTION_TREASURE.getPath());
	public static final ResourceLocation DESERT_PYRAMID = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.DESERT_PYRAMID.getPath());
	public static final ResourceLocation END_CITY_TREASURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.END_CITY_TREASURE.getPath());
	public static final ResourceLocation NETHER_BRIDGE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.NETHER_BRIDGE.getPath());
	public static final ResourceLocation SHIPWRECK_TREASURE = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.SHIPWRECK_TREASURE.getPath());
	public static final ResourceLocation SIMPLE_DUNGEON = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.SIMPLE_DUNGEON.getPath());
	public static final ResourceLocation WOODLAND_MANSION = new ResourceLocation(SophisticatedBackpacks.MOD_ID, INJECT_FOLDER + BuiltInLootTables.WOODLAND_MANSION.getPath());
	public static final Set<ResourceLocation> ALL_TABLES = Set.of(ABANDONED_MINESHAFT, BASTION_TREASURE, DESERT_PYRAMID, END_CITY_TREASURE, NETHER_BRIDGE, SHIPWRECK_TREASURE, SIMPLE_DUNGEON, WOODLAND_MANSION);

	@Override
	public void generate(BiConsumer<ResourceLocation, LootTable.Builder> tables) {
		tables.accept(SIMPLE_DUNGEON, getLootTable(92,
				getItemLootEntry(ModItems.BACKPACK.get(), 4),
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 2),
				getItemLootEntry(ModItems.PICKUP_UPGRADE.get(), 2)));
		tables.accept(ABANDONED_MINESHAFT, getLootTable(89,
				getItemLootEntry(ModItems.BACKPACK.get(), 5),
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 1),
				getItemLootEntry(ModItems.MAGNET_UPGRADE.get(), 2)));
		tables.accept(DESERT_PYRAMID, getLootTable(89,
				getItemLootEntry(ModItems.BACKPACK.get(), 5),
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 1),
				getItemLootEntry(ModItems.MAGNET_UPGRADE.get(), 2)));
		tables.accept(SHIPWRECK_TREASURE, getLootTable(92,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 4),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 2),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));
		tables.accept(WOODLAND_MANSION, getLootTable(92,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 4),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 2),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));
		tables.accept(NETHER_BRIDGE, getLootTable(90,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.FEEDING_UPGRADE.get(), 2)));
		tables.accept(BASTION_TREASURE, getLootTable(90,
				getItemLootEntry(ModItems.IRON_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.FEEDING_UPGRADE.get(), 2)));
		tables.accept(END_CITY_TREASURE, getLootTable(90,
				getItemLootEntry(ModItems.DIAMOND_BACKPACK.get(), 3),
				getItemLootEntry(ModItems.GOLD_BACKPACK.get(), 5),
				getItemLootEntry(ModItems.ADVANCED_MAGNET_UPGRADE.get(), 2)));
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
