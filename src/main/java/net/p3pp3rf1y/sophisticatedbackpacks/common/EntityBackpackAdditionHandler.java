package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.collect.ImmutableList;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.ItemStack;
import net.minecraft.item.SpawnEggItem;
import net.minecraft.loot.LootContext;
import net.minecraft.loot.LootParameterSets;
import net.minecraft.loot.LootParameters;
import net.minecraft.loot.LootTable;
import net.minecraft.potion.Effect;
import net.minecraft.potion.EffectInstance;
import net.minecraft.potion.Effects;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.common.util.FakePlayer;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class EntityBackpackAdditionHandler {
	private static final int MAX_DIFFICULTY = 3;

	private EntityBackpackAdditionHandler() {}

	private static final String SPAWNED_WITH_BACKPACK = "spawnedWithBackpack";

	static void addBackpack(MonsterEntity monster) {
		if (!Config.COMMON.entityBackpackAdditions.canWearBackpack(monster.getType())
				|| monster.world.rand.nextInt((int) (1 / Config.COMMON.entityBackpackAdditions.chance.get())) != 0) {
			return;
		}

		int backpackTierChance = monster.world.rand.nextInt(125);
		ItemStack backpack;

		int minDifficulty = 0;
		if (backpackTierChance == 0) {
			backpack = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
			minDifficulty = 3;
		} else if (backpackTierChance < 5) {
			backpack = new ItemStack(ModItems.GOLD_BACKPACK.get());
			minDifficulty = 2;
		} else if (backpackTierChance < 25) {
			backpack = new ItemStack(ModItems.IRON_BACKPACK.get());
			minDifficulty = 1;
		} else {
			backpack = new ItemStack(ModItems.BACKPACK.get());
		}

		int difficulty = Math.max(minDifficulty, monster.world.rand.nextInt(MAX_DIFFICULTY + 1));

		int finalMinDifficulty = minDifficulty;
		getSpawnEgg(monster.getType()).ifPresent(egg -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(w -> {
					w.setColors(getPrimaryColor(egg), getSecondaryColor(egg));
					setLootAndApplyPotions(monster, w, difficulty, finalMinDifficulty);
				}));
		monster.setItemStackToSlot(EquipmentSlotType.CHEST, backpack);
		monster.addTag(SPAWNED_WITH_BACKPACK);
	}

	private static Optional<SpawnEggItem> getSpawnEgg(EntityType<?> entityType) {
		Map<EntityType<?>, SpawnEggItem> eggs = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, null, "field_195987_b");
		return eggs == null ? Optional.empty() : Optional.ofNullable(eggs.get(entityType));
	}

	private static int getPrimaryColor(SpawnEggItem egg) {
		Integer primaryColor = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, egg, "field_195988_c");
		return primaryColor == null ? -1 : primaryColor;
	}

	private static int getSecondaryColor(SpawnEggItem egg) {
		Integer secondaryColor = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, egg, "field_195989_d");
		return secondaryColor == null ? -1 : secondaryColor;
	}

	private static final List<Effect> APPLICABLE_EFFECTS = ImmutableList.of(Effects.RESISTANCE, Effects.FIRE_RESISTANCE, Effects.ABSORPTION, Effects.HEALTH_BOOST,
			Effects.REGENERATION, Effects.SPEED, Effects.STRENGTH);

	private static void setLootAndApplyPotions(MonsterEntity monster, IBackpackWrapper backpackWrapper, int difficulty, int minDifficulty) {
		MinecraftServer server = monster.world.getServer();
		if (server == null) {
			return;
		}

		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.buffWithPotionEffects.get())) {
			RandHelper.getNRandomElements(APPLICABLE_EFFECTS, difficulty + 2)
					.forEach(effect -> {
						int amplifier = Math.min(minDifficulty, monster.world.rand.nextInt(difficulty + 1));
						monster.addPotionEffect(new EffectInstance(effect, 30 * 60 * 20, amplifier));
					});
		}

		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.addLoot.get())) {
			addLoot(monster, backpackWrapper, difficulty, server);
		}
	}

	private static void addLoot(MonsterEntity monster, IBackpackWrapper backpackWrapper, int difficulty, MinecraftServer server) {
		if (difficulty != 0) {
			List<ItemStack> loot = getLoot(monster, server);

			if (difficulty < MAX_DIFFICULTY) {
				loot = RandHelper.getNRandomElements(loot, (int) (loot.size() / (float) MAX_DIFFICULTY * difficulty));
			}
			BackpackInventoryHandler backpackInventory = backpackWrapper.getInventoryHandler();
			List<Integer> slots = InventoryHelper.getEmptySlotsRandomized(backpackInventory, monster.world.rand);
			InventoryHelper.shuffleItems(loot, slots.size(), monster.world.rand);

			for (ItemStack lootStack : loot) {
				if (slots.isEmpty()) {
					SophisticatedBackpacks.LOGGER.warn("Tried to over-fill backpack");
					return;
				}

				if (!lootStack.isEmpty()) {
					backpackInventory.setStackInSlot(slots.remove(slots.size() - 1), lootStack);
				}
			}
		}
	}

	private static List<ItemStack> getLoot(Entity entity, MinecraftServer server) {
		return Config.COMMON.entityBackpackAdditions.getLootTableName(entity.getType()).map(lootTableName -> {
			LootTable lootTable = server.getLootTableManager().getLootTableFromLocation(lootTableName);
			LootContext.Builder lootBuilder = (new LootContext.Builder((ServerWorld) entity.world)).withParameter(LootParameters.field_237457_g_, Vector3d.copyCentered(entity.getPosition())).withSeed(entity.world.rand.nextLong());
			List<ItemStack> lootStacks = new ArrayList<>();
			lootTable.recursiveGenerate(lootBuilder.build(LootParameterSets.CHEST), lootStacks::add);
			return lootStacks;
		}).orElse(Collections.emptyList());
	}

	static void handleBackpackDrop(LivingDropsEvent event) {
		if (event.getEntity().getTags().contains(SPAWNED_WITH_BACKPACK) && (!event.getSource().damageType.equals("player") || event.getSource().getTrueSource() instanceof FakePlayer)) {
			event.getDrops().removeIf(drop -> drop.getItem().getItem() instanceof BackpackItem);
		}
	}
}
