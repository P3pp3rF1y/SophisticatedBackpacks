package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.collect.ImmutableList;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.ai.attributes.AttributeModifier;
import net.minecraft.entity.ai.attributes.Attributes;
import net.minecraft.entity.ai.attributes.ModifiableAttributeInstance;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
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
import net.p3pp3rf1y.sophisticatedbackpacks.util.WeightedElement;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;

public class EntityBackpackAdditionHandler {
	private static final int MAX_DIFFICULTY = 3;

	private EntityBackpackAdditionHandler() {}

	private static final String SPAWNED_WITH_BACKPACK = "spawnedWithBackpack";

	private static final List<WeightedElement<Item>> HELMET_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_HELMET),
			new WeightedElement<>(4, Items.DIAMOND_HELMET),
			new WeightedElement<>(16, Items.GOLDEN_HELMET),
			new WeightedElement<>(64, Items.IRON_HELMET),
			new WeightedElement<>(256, Items.LEATHER_HELMET),
			new WeightedElement<>(1024, Items.AIR)
	);
	private static final List<WeightedElement<Item>> LEGGINGS_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_LEGGINGS),
			new WeightedElement<>(4, Items.DIAMOND_LEGGINGS),
			new WeightedElement<>(16, Items.GOLDEN_LEGGINGS),
			new WeightedElement<>(64, Items.IRON_LEGGINGS),
			new WeightedElement<>(256, Items.LEATHER_LEGGINGS),
			new WeightedElement<>(1024, Items.AIR)
	);
	private static final List<WeightedElement<Item>> BOOTS_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_BOOTS),
			new WeightedElement<>(4, Items.DIAMOND_BOOTS),
			new WeightedElement<>(16, Items.GOLDEN_BOOTS),
			new WeightedElement<>(64, Items.IRON_BOOTS),
			new WeightedElement<>(256, Items.LEATHER_BOOTS),
			new WeightedElement<>(1024, Items.AIR)
	);

	private static final List<WeightedElement<BackpackAddition>> BACKPACK_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, new BackpackAddition(ModItems.NETHERITE_BACKPACK.get(), 4,
					HELMET_CHANCES.subList(0, 2), LEGGINGS_CHANCES.subList(0, 2), BOOTS_CHANCES.subList(0, 3))),
			new WeightedElement<>(5, new BackpackAddition(ModItems.DIAMOND_BACKPACK.get(), 3,
					HELMET_CHANCES.subList(0, 3), LEGGINGS_CHANCES.subList(0, 3), BOOTS_CHANCES.subList(0, 3))),
			new WeightedElement<>(25, new BackpackAddition(ModItems.GOLD_BACKPACK.get(), 2,
					HELMET_CHANCES.subList(1, 4), LEGGINGS_CHANCES.subList(1, 4), BOOTS_CHANCES.subList(1, 4))),
			new WeightedElement<>(125, new BackpackAddition(ModItems.IRON_BACKPACK.get(), 1,
					HELMET_CHANCES.subList(2, 5), LEGGINGS_CHANCES.subList(2, 5), BOOTS_CHANCES.subList(2, 5))),
			new WeightedElement<>(625, new BackpackAddition(ModItems.BACKPACK.get(), 0,
					HELMET_CHANCES.subList(3, 6), LEGGINGS_CHANCES.subList(3, 6), BOOTS_CHANCES.subList(3, 6)))
	);

	static void addBackpack(MonsterEntity monster) {
		Random rnd = monster.world.rand;
		if (!Config.COMMON.entityBackpackAdditions.canWearBackpack(monster.getType())
				|| rnd.nextInt((int) (1 / Config.COMMON.entityBackpackAdditions.chance.get())) != 0) {
			return;
		}

		RandHelper.getRandomWeightedElement(rnd, BACKPACK_CHANCES).ifPresent(backpackAddition -> {
			ItemStack backpack = new ItemStack(backpackAddition.getBackpackItem());
			int minDifficulty = backpackAddition.getMinDifficulty();
			int difficulty = Math.max(minDifficulty, rnd.nextInt(MAX_DIFFICULTY + 1));
			equipBackpack(monster, backpack, difficulty);
			applyPotions(monster, difficulty, minDifficulty);
			raiseHealth(monster, minDifficulty);
			if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.equipWithArmor.get())) {
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getHelmetChances(), EquipmentSlotType.HEAD);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getLeggingsChances(), EquipmentSlotType.LEGS);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getBootsChances(), EquipmentSlotType.FEET);
			}
			monster.addTag(SPAWNED_WITH_BACKPACK);
		});
	}

	private static void equipArmorPiece(MonsterEntity monster, Random rnd, int minDifficulty, List<WeightedElement<Item>> armorChances, EquipmentSlotType slot) {
		RandHelper.getRandomWeightedElement(rnd, armorChances).ifPresent(armorPiece -> {
			if (armorPiece != Items.AIR) {
				ItemStack armorStack = new ItemStack(armorPiece);
				if (rnd.nextInt(6 - minDifficulty) == 0) {
					float additionalDifficulty = monster.world.getDifficultyForLocation(monster.getPosition()).getClampedAdditionalDifficulty();
					int level = (int) (5F + additionalDifficulty * 18F + minDifficulty * 6);
					EnchantmentHelper.addRandomEnchantment(rnd, armorStack, level, true);
				}
				monster.setItemStackToSlot(slot, armorStack);
			}
		});
	}

	private static void equipBackpack(MonsterEntity monster, ItemStack backpack, int difficulty) {
		getSpawnEgg(monster.getType()).ifPresent(egg -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(w -> {
					w.setColors(getPrimaryColor(egg), getSecondaryColor(egg));
					setLoot(monster, w, difficulty);
				}));
		monster.setItemStackToSlot(EquipmentSlotType.CHEST, backpack);
	}

	private static void raiseHealth(MonsterEntity monster, int minDifficulty) {
		if (Boolean.FALSE.equals(Config.COMMON.entityBackpackAdditions.buffHealth.get())) {
			return;
		}
		ModifiableAttributeInstance maxHealth = monster.getAttribute(Attributes.MAX_HEALTH);
		if (maxHealth != null) {
			double healthAddition = maxHealth.getBaseValue() * 0.5D * minDifficulty;
			if (healthAddition > 0.1D) {
				maxHealth.applyPersistentModifier(new AttributeModifier("Backpack bearer health bonus", healthAddition, AttributeModifier.Operation.ADDITION));
			}
		}
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

	private static void setLoot(MonsterEntity monster, IBackpackWrapper backpackWrapper, int difficulty) {
		MinecraftServer server = monster.world.getServer();
		if (server == null) {
			return;
		}

		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.addLoot.get())) {
			addLoot(monster, backpackWrapper, difficulty, server);
		}
	}

	private static void applyPotions(MonsterEntity monster, int difficulty, int minDifficulty) {
		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.buffWithPotionEffects.get())) {
			RandHelper.getNRandomElements(APPLICABLE_EFFECTS, difficulty + 2)
					.forEach(effect -> {
						int amplifier = Math.max(minDifficulty, monster.world.rand.nextInt(difficulty + 1));
						monster.addPotionEffect(new EffectInstance(effect, 30 * 60 * 20, amplifier));
					});
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
		if (event.getEntity().getTags().contains(SPAWNED_WITH_BACKPACK) && (!(event.getSource().getTrueSource() instanceof PlayerEntity) || event.getSource().getTrueSource() instanceof FakePlayer)) {
			event.getDrops().removeIf(drop -> drop.getItem().getItem() instanceof BackpackItem);
		}
	}

	private static class BackpackAddition {
		private final Item backpackItem;
		private final int minDifficulty;

		private final List<WeightedElement<Item>> helmetChances;

		public List<WeightedElement<Item>> getHelmetChances() {
			return helmetChances;
		}

		public List<WeightedElement<Item>> getLeggingsChances() {
			return leggingsChances;
		}

		public List<WeightedElement<Item>> getBootsChances() {
			return bootsChances;
		}

		private final List<WeightedElement<Item>> leggingsChances;
		private final List<WeightedElement<Item>> bootsChances;

		private BackpackAddition(Item backpackItem, int minDifficulty, List<WeightedElement<Item>> helmetChances, List<WeightedElement<Item>> leggingsChances, List<WeightedElement<Item>> bootsChances) {
			this.backpackItem = backpackItem;
			this.minDifficulty = minDifficulty;
			this.helmetChances = helmetChances;
			this.leggingsChances = leggingsChances;
			this.bootsChances = bootsChances;
		}

		public Item getBackpackItem() {
			return backpackItem;
		}

		public int getMinDifficulty() {
			return minDifficulty;
		}
	}
}
