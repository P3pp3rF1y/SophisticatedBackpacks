package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.storage.loot.LootContext;
import net.minecraft.world.level.storage.loot.LootTable;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;
import net.minecraft.world.level.storage.loot.parameters.LootContextParams;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

public class LootHelper {
	private LootHelper() {}

	public static List<ItemStack> getLoot(ResourceLocation lootTableName, MinecraftServer server, ServerLevel world, Entity entity) {
		LootTable lootTable = server.getLootTables().get(lootTableName);
		LootContext.Builder lootBuilder = (new LootContext.Builder(world)).withParameter(LootContextParams.ORIGIN, Vec3.atCenterOf(entity.blockPosition())).withOptionalRandomSeed(world.random.nextLong());
		List<ItemStack> lootStacks = new ArrayList<>();
		lootTable.getRandomItemsRaw(lootBuilder.create(LootContextParamSets.CHEST), lootStacks::add);
		return lootStacks;
	}

	public static void fillWithLoot(Random rand, List<ItemStack> loot, IItemHandlerModifiable inventory) {
		List<Integer> slots = InventoryHelper.getEmptySlotsRandomized(inventory, rand);
		InventoryHelper.shuffleItems(loot, slots.size(), rand);

		for (ItemStack lootStack : loot) {
			if (slots.isEmpty()) {
				SophisticatedBackpacks.LOGGER.warn("Tried to over-fill backpack");
				return;
			}

			if (!lootStack.isEmpty()) {
				inventory.setStackInSlot(slots.remove(slots.size() - 1), lootStack);
			}
		}
	}
}
