package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.collect.ImmutableList;
import net.minecraft.enchantment.EnchantmentHelper;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.ai.attributes.AttributeModifier;
import net.minecraft.entity.ai.attributes.Attributes;
import net.minecraft.entity.ai.attributes.ModifiableAttributeInstance;
import net.minecraft.entity.monster.CreeperEntity;
import net.minecraft.entity.monster.MonsterEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.item.MusicDiscItem;
import net.minecraft.item.SpawnEggItem;
import net.minecraft.potion.Effect;
import net.minecraft.potion.EffectInstance;
import net.minecraft.potion.Effects;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.SoundEvent;
import net.minecraftforge.common.util.FakePlayer;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingEvent;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RandHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WeightedElement;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Random;
import java.util.Set;

public class EntityBackpackAdditionHandler {
	private static final int MAX_DIFFICULTY = 3;

	private EntityBackpackAdditionHandler() {}

	private static final String SPAWNED_WITH_BACKPACK = "spawnedWithBackpack";
	private static final String SPAWNED_WITH_JUKEBOX_UPGRADE = SophisticatedBackpacks.MOD_ID + ":jukebox";

	private static final List<WeightedElement<Item>> HELMET_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_HELMET),
			new WeightedElement<>(3, Items.DIAMOND_HELMET),
			new WeightedElement<>(9, Items.GOLDEN_HELMET),
			new WeightedElement<>(27, Items.IRON_HELMET),
			new WeightedElement<>(81, Items.LEATHER_HELMET),
			new WeightedElement<>(243, Items.AIR)
	);
	private static final List<WeightedElement<Item>> LEGGINGS_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_LEGGINGS),
			new WeightedElement<>(3, Items.DIAMOND_LEGGINGS),
			new WeightedElement<>(9, Items.GOLDEN_LEGGINGS),
			new WeightedElement<>(27, Items.IRON_LEGGINGS),
			new WeightedElement<>(81, Items.LEATHER_LEGGINGS),
			new WeightedElement<>(243, Items.AIR)
	);
	private static final List<WeightedElement<Item>> BOOTS_CHANCES = ImmutableList.of(
			new WeightedElement<>(1, Items.NETHERITE_BOOTS),
			new WeightedElement<>(3, Items.DIAMOND_BOOTS),
			new WeightedElement<>(9, Items.GOLDEN_BOOTS),
			new WeightedElement<>(27, Items.IRON_BOOTS),
			new WeightedElement<>(81, Items.LEATHER_BOOTS),
			new WeightedElement<>(243, Items.AIR)
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
			equipBackpack(monster, backpack, difficulty, Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.playJukebox.get()) && rnd.nextInt(4) == 0);
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

	private static void equipBackpack(MonsterEntity monster, ItemStack backpack, int difficulty, boolean playMusicDisc) {
		getSpawnEgg(monster.getType()).ifPresent(egg -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(w -> {
					w.setColors(getPrimaryColor(egg), getSecondaryColor(egg));
					setLoot(monster, w, difficulty);
					if (playMusicDisc) {
						w.getInventoryHandler(); //just to assign uuid and real upgrade handler
						if (w.getUpgradeHandler().getSlots() > 0) {
							monster.addTag(SPAWNED_WITH_JUKEBOX_UPGRADE);
							addJukeboxUpgradeAndRandomDisc(monster, w);
						}
					}
				}));
		monster.setItemStackToSlot(EquipmentSlotType.CHEST, backpack);
	}

	private static void addJukeboxUpgradeAndRandomDisc(MonsterEntity monster, IBackpackWrapper w) {
		w.getUpgradeHandler().setStackInSlot(0, new ItemStack(ModItems.JUKEBOX_UPGRADE.get()));
		Iterator<JukeboxUpgradeItem.Wrapper> it = w.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).iterator();
		if (it.hasNext()) {
			JukeboxUpgradeItem.Wrapper wrapper = it.next();
			List<MusicDiscItem> musicDiscs = getMusicDiscs();
			wrapper.setDisc(new ItemStack(musicDiscs.get(monster.world.rand.nextInt(musicDiscs.size()))));
		}
	}

	private static List<MusicDiscItem> musicDiscs = null;

	private static List<MusicDiscItem> getMusicDiscs() {
		if (musicDiscs == null) {
			Map<SoundEvent, MusicDiscItem> records = ObfuscationReflectionHelper.getPrivateValue(MusicDiscItem.class, null, "field_150928_b");
			if (records == null) {
				musicDiscs = new ArrayList<>();
			} else {
				Set<String> blockedDiscs = new HashSet<>(Config.COMMON.entityBackpackAdditions.discBlockList.get());
				musicDiscs = new ArrayList<>();
				records.forEach((sound, musicDisc) -> {
					//noinspection ConstantConditions - by this point the disc has registry name
					if (!blockedDiscs.contains(musicDisc.getRegistryName().toString())) {
						musicDiscs.add(musicDisc);
					}
				});
			}
		}

		return musicDiscs;
	}

	private static void raiseHealth(MonsterEntity monster, int minDifficulty) {
		if (Boolean.FALSE.equals(Config.COMMON.entityBackpackAdditions.buffHealth.get())) {
			return;
		}
		ModifiableAttributeInstance maxHealth = monster.getAttribute(Attributes.MAX_HEALTH);
		if (maxHealth != null) {
			double healthAddition = maxHealth.getBaseValue() * minDifficulty;
			if (healthAddition > 0.1D) {
				maxHealth.applyPersistentModifier(new AttributeModifier("Backpack bearer health bonus", healthAddition, AttributeModifier.Operation.ADDITION));
			}
			monster.setHealth(monster.getMaxHealth());
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

	private static final List<ApplicableEffect> APPLICABLE_EFFECTS = ImmutableList.of(
			new ApplicableEffect(Effects.RESISTANCE, 3),
			new ApplicableEffect(Effects.FIRE_RESISTANCE),
			new ApplicableEffect(Effects.ABSORPTION),
			new ApplicableEffect(Effects.HEALTH_BOOST),
			new ApplicableEffect(Effects.REGENERATION),
			new ApplicableEffect(Effects.SPEED),
			new ApplicableEffect(Effects.STRENGTH));

	private static void setLoot(MonsterEntity monster, IBackpackWrapper backpackWrapper, int difficulty) {
		MinecraftServer server = monster.world.getServer();
		if (server == null) {
			return;
		}

		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.addLoot.get())) {
			addLoot(monster, backpackWrapper, difficulty);
		}
	}

	private static void applyPotions(MonsterEntity monster, int difficulty, int minDifficulty) {
		if (Boolean.TRUE.equals(Config.COMMON.entityBackpackAdditions.buffWithPotionEffects.get())) {
			RandHelper.getNRandomElements(APPLICABLE_EFFECTS, difficulty + 2)
					.forEach(applicableEffect -> {
						int amplifier = Math.min(Math.max(minDifficulty, monster.world.rand.nextInt(difficulty + 1)), applicableEffect.getMaxAmplifier());
						monster.addPotionEffect(new EffectInstance(applicableEffect.getEffect(), 30 * 60 * 20, amplifier));
					});
		}
	}

	private static void addLoot(MonsterEntity monster, IBackpackWrapper backpackWrapper, int difficulty) {
		if (difficulty != 0) {
			Config.COMMON.entityBackpackAdditions.getLootTableName(monster.getType()).ifPresent(lootTableName -> {
				float lootPercentage = (float) difficulty / MAX_DIFFICULTY;
				backpackWrapper.setLoot(lootTableName, lootPercentage);
			});
		}
	}

	static void handleBackpackDrop(LivingDropsEvent event) {
		if (event.getEntity().getTags().contains(SPAWNED_WITH_BACKPACK) && (!(event.getSource().getTrueSource() instanceof PlayerEntity) || event.getSource().getTrueSource() instanceof FakePlayer)) {
			event.getDrops().removeIf(drop -> {
				if (drop.getItem().getItem() instanceof BackpackItem) {
					removeContentsUuid(drop.getItem());
					return true;
				}
				return false;
			});
		}
	}

	public static void removeBeneficialEffects(CreeperEntity creeper) {
		if (creeper.getTags().contains(SPAWNED_WITH_BACKPACK)) {
			creeper.getActivePotionEffects().removeIf(e -> e.getPotion().isBeneficial());
		}
	}

	public static void removeBackpackUuid(MonsterEntity entity) {
		if (!entity.getShouldBeDead() || !entity.getTags().contains(SPAWNED_WITH_BACKPACK)) {
			return;
		}

		ItemStack stack = entity.getItemStackFromSlot(EquipmentSlotType.CHEST);
		removeContentsUuid(stack);
	}

	private static void removeContentsUuid(ItemStack stack) {
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(backpackWrapper -> backpackWrapper.getContentsUuid().ifPresent(uuid -> BackpackStorage.get().removeBackpackContents(uuid)));
	}

	public static void onLivingUpdate(LivingEvent.LivingUpdateEvent event) {
		LivingEntity entity = event.getEntityLiving();
		if (!entity.getTags().contains(SPAWNED_WITH_JUKEBOX_UPGRADE)) {
			return;
		}
		entity.getItemStackFromSlot(EquipmentSlotType.CHEST).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(backpackWrapper -> backpackWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
					if (wrapper.isPlaying()) {
						wrapper.tick(entity, entity.world, entity.getPosition());
					} else {
						wrapper.play(entity);
					}
				}));
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

	private static class ApplicableEffect {
		private final Effect effect;

		private final int maxAmplifier;

		private ApplicableEffect(Effect effect) {
			this(effect, Integer.MAX_VALUE);
		}

		private ApplicableEffect(Effect effect, int maxAmplifier) {
			this.effect = effect;
			this.maxAmplifier = maxAmplifier;
		}

		public Effect getEffect() {
			return effect;
		}

		public int getMaxAmplifier() {
			return maxAmplifier;
		}
	}
}
