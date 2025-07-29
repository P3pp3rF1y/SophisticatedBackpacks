package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.primitives.Ints;
import net.minecraft.core.Holder;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.server.MinecraftServer;
import net.minecraft.tags.EnchantmentTags;
import net.minecraft.util.ARGB;
import net.minecraft.util.RandomSource;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.AttributeInstance;
import net.minecraft.world.entity.ai.attributes.AttributeModifier;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.item.ItemEntity;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.entity.raid.Raider;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.ItemContainerContents;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.neoforged.neoforge.common.Tags;
import net.neoforged.neoforge.common.util.FakePlayer;
import net.neoforged.neoforge.event.entity.living.LivingDropsEvent;
import net.neoforged.neoforge.event.tick.EntityTickEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RandHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WeightedElement;

import java.util.*;

public class EntityBackpackAdditionHandler {
	private static final int MAX_DIFFICULTY = 3;
	private static final float MAX_LOCAL_DIFFICULTY = 6.75f;

	private EntityBackpackAdditionHandler() {
	}

	private static final String SPAWNED_WITH_BACKPACK = "spawnedWithBackpack";
	private static final String SPAWNED_WITH_JUKEBOX_UPGRADE = SophisticatedBackpacks.MOD_ID + ":jukebox";

	private static final List<WeightedElement<Item>> HELMET_CHANCES = List.of(
			new WeightedElement<>(1, Items.NETHERITE_HELMET),
			new WeightedElement<>(3, Items.DIAMOND_HELMET),
			new WeightedElement<>(9, Items.GOLDEN_HELMET),
			new WeightedElement<>(27, Items.IRON_HELMET),
			new WeightedElement<>(81, Items.LEATHER_HELMET)
	);
	private static final List<WeightedElement<Item>> LEGGINGS_CHANCES = List.of(
			new WeightedElement<>(1, Items.NETHERITE_LEGGINGS),
			new WeightedElement<>(3, Items.DIAMOND_LEGGINGS),
			new WeightedElement<>(9, Items.GOLDEN_LEGGINGS),
			new WeightedElement<>(27, Items.IRON_LEGGINGS),
			new WeightedElement<>(81, Items.LEATHER_LEGGINGS)
	);
	private static final List<WeightedElement<Item>> BOOTS_CHANCES = List.of(
			new WeightedElement<>(1, Items.NETHERITE_BOOTS),
			new WeightedElement<>(3, Items.DIAMOND_BOOTS),
			new WeightedElement<>(9, Items.GOLDEN_BOOTS),
			new WeightedElement<>(27, Items.IRON_BOOTS),
			new WeightedElement<>(81, Items.LEATHER_BOOTS)
	);

	private static final Map<Item, Float> dropChanceMultiplier = Map.of(
			ModItems.BACKPACK.get(), 1F,
			ModItems.COPPER_BACKPACK.get(), 1.25F,
			ModItems.IRON_BACKPACK.get(), 1.5F,
			ModItems.GOLD_BACKPACK.get(), 3F,
			ModItems.DIAMOND_BACKPACK.get(), 4.5F,
			ModItems.NETHERITE_BACKPACK.get(), 6F
	);

	private static final List<WeightedElement<BackpackAddition>> BACKPACK_CHANCES = List.of(
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.leatherWeight.getAsInt(), new BackpackAddition(ModItems.BACKPACK.get(), 0,
					HELMET_CHANCES.subList(3, 5), LEGGINGS_CHANCES.subList(3, 5), BOOTS_CHANCES.subList(3, 5))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.copperWeight.getAsInt(), new BackpackAddition(ModItems.COPPER_BACKPACK.get(), 1,
					HELMET_CHANCES.subList(2, 4), LEGGINGS_CHANCES.subList(3, 5), BOOTS_CHANCES.subList(3, 5))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.ironWeight.getAsInt(), new BackpackAddition(ModItems.IRON_BACKPACK.get(), 1,
					HELMET_CHANCES.subList(2, 4), LEGGINGS_CHANCES.subList(2, 4), BOOTS_CHANCES.subList(2, 4))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.goldWeight.getAsInt(), new BackpackAddition(ModItems.GOLD_BACKPACK.get(), 2,
					HELMET_CHANCES.subList(1, 3), LEGGINGS_CHANCES.subList(1, 3), BOOTS_CHANCES.subList(1, 3))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.diamondWeight.getAsInt(), new BackpackAddition(ModItems.DIAMOND_BACKPACK.get(), 3,
					HELMET_CHANCES.subList(0, 2), LEGGINGS_CHANCES.subList(0, 2), BOOTS_CHANCES.subList(0, 2))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.netheriteWeight.getAsInt(), new BackpackAddition(ModItems.NETHERITE_BACKPACK.get(), 4,
					HELMET_CHANCES.subList(0, 1), LEGGINGS_CHANCES.subList(0, 1), BOOTS_CHANCES.subList(0, 1)))
	);

	private static final Map<Integer, List<WeightedElement<BackpackAddition>>> DIFFICULTY_BACKPACK_CHANCES = Map.of(
			0, BACKPACK_CHANCES,
			1, BACKPACK_CHANCES.subList(Config.SERVER.entityBackpackAdditions.minBackpackTierMidDifficulty.getAsInt(), 6),
			2, BACKPACK_CHANCES.subList(Config.SERVER.entityBackpackAdditions.minBackpackTierHighDifficulty.getAsInt(), 6)
	);

	static void addBackpack(Monster monster, LevelAccessor level) {
		RandomSource rnd = level.getRandom();
		if (!Config.SERVER.entityBackpackAdditions.canWearBackpack(monster.getType())
				|| rnd.nextInt((int) (1 / Config.SERVER.entityBackpackAdditions.chance.get())) != 0 || (monster instanceof Raider raider && raider.getCurrentRaid() != null)) {
			return;
		}

		int difficultyIndex = 0;
		if (Config.SERVER.entityBackpackAdditions.localDifficultyEffectsBackpackSpawns.getAsBoolean()) {
			float localDifficulty = level.getCurrentDifficultyAt(monster.blockPosition()).getEffectiveDifficulty();
			difficultyIndex = Ints.constrainToRange((int) Math.floor(DIFFICULTY_BACKPACK_CHANCES.size() / MAX_LOCAL_DIFFICULTY * localDifficulty - 0.1f), 0, DIFFICULTY_BACKPACK_CHANCES.size() - 1);
		}

		RandHelper.getRandomWeightedElement(rnd, DIFFICULTY_BACKPACK_CHANCES.get(difficultyIndex)).ifPresent(backpackAddition -> {
			ItemStack backpack = new ItemStack(backpackAddition.getBackpackItem());
			int minDifficulty = backpackAddition.getMinDifficulty();
			int difficulty = Math.max(minDifficulty, rnd.nextInt(MAX_DIFFICULTY + 1));
			equipBackpack(monster, backpack, difficulty, Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.playJukebox.get()) && rnd.nextInt(4) == 0, level, rnd);
			applyPotions(monster, difficulty, minDifficulty, rnd);
			raiseHealth(monster, minDifficulty);
			if (Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.equipWithArmor.get())) {
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getHelmetChances(), EquipmentSlot.HEAD, level);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getLeggingsChances(), EquipmentSlot.LEGS, level);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getBootsChances(), EquipmentSlot.FEET, level);
			}
			monster.addTag(SPAWNED_WITH_BACKPACK);
		});
	}

	private static void equipArmorPiece(Monster monster, RandomSource rnd, int minDifficulty, List<WeightedElement<Item>> armorChances, EquipmentSlot slot, LevelAccessor level) {
		RandHelper.getRandomWeightedElement(rnd, armorChances).ifPresent(armorPiece -> {
			if (armorPiece != Items.AIR) {
				ItemStack armorStack = new ItemStack(armorPiece);
				if (rnd.nextInt(6 - minDifficulty) == 0) {
					float additionalDifficulty = level.getCurrentDifficultyAt(monster.blockPosition()).getSpecialMultiplier();
					int enchantmentLevel = (int) (5F + additionalDifficulty * 18F + minDifficulty * 6);
					EnchantmentHelper.enchantItem(rnd, armorStack, enchantmentLevel, level.registryAccess(),
							level.registryAccess().lookupOrThrow(Registries.ENCHANTMENT).get(EnchantmentTags.ON_MOB_SPAWN_EQUIPMENT));
				}
				monster.setItemSlot(slot, armorStack);
			}
		});
	}

	private static void equipBackpack(Monster monster, ItemStack backpack, int difficulty, boolean playMusicDisc, LevelAccessor level, RandomSource rnd) {
		IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
		EntityBackpackColors.BackpackColors colors = EntityBackpackColors.getBackpackColors(monster.getType());
		wrapper.setColors(ARGB.opaque(colors.main()), ARGB.opaque(colors.accent()));
		setLoot(monster, wrapper, difficulty, level);
		if (playMusicDisc) {
			wrapper.getInventoryHandler(); //just to assign uuid and real upgrade handler
			if (wrapper.getUpgradeHandler().getSlots() > 0) {
				monster.addTag(SPAWNED_WITH_JUKEBOX_UPGRADE);
				addJukeboxUpgradeAndRandomDisc(level.getRandom(), wrapper, rnd);
			}
		}
		monster.setItemSlot(EquipmentSlot.CHEST, backpack);
		monster.setDropChance(EquipmentSlot.CHEST, 0);
	}

	private static void addJukeboxUpgradeAndRandomDisc(RandomSource random, IStorageWrapper w, RandomSource rnd) {
		boolean advancedJukebox = random.nextFloat() < 0.25;
		w.getUpgradeHandler().setStackInSlot(0, new ItemStack(advancedJukebox ? ModItems.ADVANCED_JUKEBOX_UPGRADE.get() : ModItems.JUKEBOX_UPGRADE.get()));
		Iterator<JukeboxUpgradeWrapper> it = w.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).iterator();
		if (it.hasNext()) {
			List<Item> musicDiscs = getMusicDiscs();
			if (musicDiscs.isEmpty()) {
				SophisticatedBackpacks.LOGGER.warn("No music discs found to add to jukebox upgrade, either there are none registered or they are all blocked");
				return;
			}

			JukeboxUpgradeWrapper wrapper = it.next();
			int numberOfDiscs = advancedJukebox ? random.nextInt(wrapper.getDiscInventory().getSlots() / 3) + 1 : 1;
			for (int i = 0; i < numberOfDiscs; i++) {
				wrapper.getDiscInventory().insertItem(i, new ItemStack(getMusicDiscs().get(rnd.nextInt(musicDiscs.size())), 1), false);
			}
		}
	}

	private static List<Item> musicDiscs = null;

	private static List<Item> getMusicDiscs() {
		if (musicDiscs == null) {
			BuiltInRegistries.ITEM.get(Tags.Items.MUSIC_DISCS).ifPresentOrElse(records -> {
				Set<String> blockedDiscs = new HashSet<>(Config.SERVER.entityBackpackAdditions.discBlockList.get());
				musicDiscs = new ArrayList<>();
				records.forEach(musicDisc -> {
					//noinspection ConstantConditions - by this point the disc has registry name
					if (!blockedDiscs.contains(musicDisc.getKey().location().toString())) {
						musicDiscs.add(musicDisc.value());
					}
				});
			}, () -> musicDiscs = Collections.emptyList());
		}

		return musicDiscs;
	}

	private static void raiseHealth(Monster monster, int minDifficulty) {
		if (Boolean.FALSE.equals(Config.SERVER.entityBackpackAdditions.buffHealth.get())) {
			return;
		}
		AttributeInstance maxHealth = monster.getAttribute(Attributes.MAX_HEALTH);
		if (maxHealth != null) {
			double healthAddition = maxHealth.getBaseValue() * minDifficulty;
			if (healthAddition > 0.1D) {
				maxHealth.addPermanentModifier(new AttributeModifier(SophisticatedBackpacks.getRL("backpack_bearer_health_bonus"), healthAddition, AttributeModifier.Operation.ADD_VALUE));
			}
			monster.setHealth(monster.getMaxHealth());
		}
	}

	private static final List<ApplicableEffect> APPLICABLE_EFFECTS = List.of(
			new ApplicableEffect(List.of(MobEffects.DAMAGE_RESISTANCE, MobEffects.REGENERATION), 1),
			new ApplicableEffect(MobEffects.FIRE_RESISTANCE),
			new ApplicableEffect(MobEffects.ABSORPTION),
			new ApplicableEffect(MobEffects.HEALTH_BOOST),
			new ApplicableEffect(MobEffects.MOVEMENT_SPEED),
			new ApplicableEffect(MobEffects.DAMAGE_BOOST));

	private static void setLoot(Monster monster, IBackpackWrapper backpackWrapper, int difficulty, LevelAccessor level) {
		MinecraftServer server = level.getServer();
		if (server == null) {
			return;
		}

		if (Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.addLoot.get())) {
			addLoot(monster, backpackWrapper, difficulty);
		}
	}

	private static void applyPotions(Monster monster, int difficulty, int minDifficulty, RandomSource rnd) {
		if (Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.buffWithPotionEffects.get())) {
			RandHelper.getNRandomElements(APPLICABLE_EFFECTS, difficulty + 2)
					.forEach(applicableEffect -> {
						int amplifier = Math.min(Math.max(minDifficulty, rnd.nextInt(difficulty + 1)), applicableEffect.getMaxAmplifier());
						monster.addEffect(new MobEffectInstance(applicableEffect.getRandomEffect(rnd), 30 * 60 * 20, amplifier));
					});
		}
	}

	private static void addLoot(Monster monster, IBackpackWrapper backpackWrapper, int difficulty) {
		if (difficulty != 0) {
			Config.SERVER.entityBackpackAdditions.getLootTableName(monster.getType()).ifPresent(lootTableName -> {
				float lootFactor = (float) difficulty / MAX_DIFFICULTY;
				backpackWrapper.setLoot(lootTableName, lootFactor);
			});
		}
	}

	static void handleBackpackDrop(LivingDropsEvent event) {
		if (event.getEntity().getTags().contains(SPAWNED_WITH_BACKPACK)) {
			LivingEntity mob = event.getEntity();
			ItemStack backpack = mob.getItemBySlot(EquipmentSlot.CHEST);
			Config.Server.EntityBackpackAdditionsConfig additionsConfig = Config.SERVER.entityBackpackAdditions;
			if (shouldDropBackpack(event, additionsConfig, mob, backpack)) {
				putJukeboxItemsInContainerAndRemoveStorageUuid(event, backpack);

				ItemEntity backpackEntity = new ItemEntity(mob.level(), mob.getX(), mob.getY(), mob.getZ(), backpack);
				event.getDrops().add(backpackEntity);
				mob.setItemSlot(EquipmentSlot.CHEST, ItemStack.EMPTY);
				event.getEntity().getTags().remove(SPAWNED_WITH_BACKPACK);
			} else {
				removeContentsUuid(backpack);
			}
		}
	}

	private static void putJukeboxItemsInContainerAndRemoveStorageUuid(LivingDropsEvent event, ItemStack backpack) {
		if (event.getEntity().getTags().remove(SPAWNED_WITH_JUKEBOX_UPGRADE)) {
			List<ItemStack> inventoryItems = new ArrayList<>();
			IBackpackWrapper backpackwrapper = BackpackWrapper.fromStack(backpack);
			backpackwrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
				InventoryHelper.iterate(wrapper.getDiscInventory(), (slot, stack) -> {
					if (!stack.isEmpty()) {
						inventoryItems.add(wrapper.getDiscInventory().extractItem(slot, stack.getCount(), false));
					}
				});
			});
			InventoryHelper.iterate(backpackwrapper.getUpgradeHandler(), (slot, stack) -> {
				if (!stack.isEmpty()) {
					inventoryItems.add(backpackwrapper.getUpgradeHandler().extractItem(slot, stack.getCount(), false));
				}
			});
			UUID backpackUuid = backpack.remove(ModCoreDataComponents.STORAGE_UUID);
			if (backpackUuid != null) {
				BackpackStorage.get().removeBackpackContents(backpackUuid);
			}
			backpack.set(DataComponents.CONTAINER, ItemContainerContents.fromItems(inventoryItems));
		}
	}

	private static boolean shouldDropBackpack(LivingDropsEvent event, Config.Server.EntityBackpackAdditionsConfig additionsConfig, LivingEntity mob, ItemStack backpack) {
		if (!(event.getSource().getEntity() instanceof Player player)) {
			return false;
		}
		if (!Boolean.TRUE.equals(additionsConfig.dropToFakePlayers.get()) && event.getSource().getEntity() instanceof FakePlayer) {
			return false;
		}
		float lootingChanceMultiplier = dropChanceMultiplier.getOrDefault(backpack.getItem(), 1F);
		int lootingLevel = player.level().registryAccess().lookup(Registries.ENCHANTMENT).map(registry -> player.getMainHandItem().getEnchantmentLevel(registry.getOrThrow(Enchantments.LOOTING))).orElse(0);
		return mob.level().random.nextFloat() < (additionsConfig.backpackDropChance.get() + lootingLevel * additionsConfig.lootingChanceIncreasePerLevel.get()) * lootingChanceMultiplier;
	}

	public static void removeBeneficialEffects(Creeper creeper) {
		if (creeper.getTags().contains(SPAWNED_WITH_BACKPACK)) {
			creeper.getActiveEffects().removeIf(e -> e.getEffect().value().isBeneficial());
		}
	}

	public static void removeBackpackUuid(Monster entity, Level level) {
		if (level.isClientSide() || !entity.getTags().contains(SPAWNED_WITH_BACKPACK)) {
			return;
		}

		ItemStack stack = entity.getItemBySlot(EquipmentSlot.CHEST);
		removeContentsUuid(stack);
	}

	private static void removeContentsUuid(ItemStack stack) {
		BackpackWrapper.fromStack(stack).getContentsUuid().ifPresent(uuid -> BackpackStorage.get().removeBackpackContents(uuid));
	}

	public static void onLivingUpdate(EntityTickEvent.Post event) {
		Entity entity = event.getEntity();
		if (!(entity instanceof LivingEntity livingEntity) || !entity.getTags().contains(SPAWNED_WITH_JUKEBOX_UPGRADE)) {
			return;
		}
		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(livingEntity.getItemBySlot(EquipmentSlot.CHEST));
		backpackWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
			if (wrapper.isPlaying()) {
				wrapper.tick(livingEntity, entity.level(), entity.blockPosition());
			} else {
				wrapper.play(livingEntity);
			}
		});
	}

	private record BackpackAddition(Item backpackItem, int minDifficulty,
									List<WeightedElement<Item>> helmetChances,
									List<WeightedElement<Item>> leggingsChances,
									List<WeightedElement<Item>> bootsChances) {
		public List<WeightedElement<Item>> getHelmetChances() {
			return helmetChances;
		}

		public List<WeightedElement<Item>> getLeggingsChances() {
			return leggingsChances;
		}

		public List<WeightedElement<Item>> getBootsChances() {
			return bootsChances;
		}

		public Item getBackpackItem() {
			return backpackItem;
		}

		public int getMinDifficulty() {
			return minDifficulty;
		}
	}

	private static class ApplicableEffect {
		private final List<Holder<MobEffect>> effects;

		private final int maxAmplifier;

		private ApplicableEffect(Holder<MobEffect> effect) {
			this(List.of(effect), Integer.MAX_VALUE);
		}

		private ApplicableEffect(List<Holder<MobEffect>> effects, int maxAmplifier) {
			this.effects = effects;
			this.maxAmplifier = maxAmplifier;
		}

		public Holder<MobEffect> getRandomEffect(RandomSource random) {
			return effects.get(random.nextInt(effects.size()));
		}

		public int getMaxAmplifier() {
			return maxAmplifier;
		}
	}
}
