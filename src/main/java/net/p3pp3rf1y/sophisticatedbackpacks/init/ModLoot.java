package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.core.Registry;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.storage.loot.LootPool;
import net.minecraft.world.level.storage.loot.entries.LootPoolEntryContainer;
import net.minecraft.world.level.storage.loot.entries.LootTableReference;
import net.minecraft.world.level.storage.loot.functions.LootItemFunctionType;
import net.minecraft.world.level.storage.loot.providers.number.UniformGenerator;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.LootTableLoadEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.data.CopyBackpackDataFunction;

import java.util.List;

public class ModLoot {
	private ModLoot() {}

	public static final LootItemFunctionType COPY_BACKPACK_DATA = new LootItemFunctionType(new CopyBackpackDataFunction.Serializer());
	private static final List<String> CHEST_TABLES = List.of("abandoned_mineshaft", "bastion_treasure", "desert_pyramid", "end_city_treasure", "nether_bridge", "shipwreck_treasure", "simple_dungeon", "woodland_mansion");

	public static void init() {
		Registry.register(Registry.LOOT_FUNCTION_TYPE, new ResourceLocation(SophisticatedBackpacks.MOD_ID, "copy_backpack_data"), COPY_BACKPACK_DATA);
		MinecraftForge.EVENT_BUS.addListener(ModLoot::lootLoad);
	}

	public static void lootLoad(LootTableLoadEvent evt) {
		if (Boolean.FALSE.equals(Config.COMMON.chestLootEnabled.get())) {
			return;
		}

		String chestsPrefix = "minecraft:chests/";
		String name = evt.getName().toString();

		if (name.startsWith(chestsPrefix) && CHEST_TABLES.contains(name.substring(chestsPrefix.length()))) {
			String file = name.substring("minecraft:".length());
			evt.getTable().addPool(getInjectPool(file));
		}
	}

	private static LootPool getInjectPool(String entryName) {
		return LootPool.lootPool().add(getInjectEntry(entryName)).setBonusRolls(UniformGenerator.between(0, 1)).name("sophisticatedbackpacks_inject_pool").build();
	}

	private static LootPoolEntryContainer.Builder<?> getInjectEntry(String name) {
		return LootTableReference.lootTableReference(new ResourceLocation(SophisticatedBackpacks.MOD_ID, "inject/" + name)).setWeight(1);
	}
}
