package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.primitives.Ints;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.RandomSource;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.*;
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
import net.minecraft.world.item.SpawnEggItem;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.minecraftforge.common.util.FakePlayer;
import net.minecraftforge.event.entity.living.LivingDropsEvent;
import net.minecraftforge.event.entity.living.LivingEvent;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.items.ItemHandlerHelper;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.DiscHandlerRegistry;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.VanillaDiscHandler;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RandHelper;
import net.p3pp3rf1y.sophisticatedcore.util.WeightedElement;

import java.util.*;

public class EntityBackpackAdditionHandler {
	private static final int MAX_DIFFICULTY = 3;
	private static final float MAX_LOCAL_DIFFICULTY = 6.75f;

	private EntityBackpackAdditionHandler() {}

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
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.leatherWeight.get(), new BackpackAddition(ModItems.BACKPACK.get(), 0,
					HELMET_CHANCES.subList(3, 5), LEGGINGS_CHANCES.subList(3, 5), BOOTS_CHANCES.subList(3, 5))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.copperWeight.get(), new BackpackAddition(ModItems.COPPER_BACKPACK.get(), 1,
					HELMET_CHANCES.subList(2, 4), LEGGINGS_CHANCES.subList(3, 5), BOOTS_CHANCES.subList(3, 5))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.ironWeight.get(), new BackpackAddition(ModItems.IRON_BACKPACK.get(), 1,
					HELMET_CHANCES.subList(2, 4), LEGGINGS_CHANCES.subList(2, 4), BOOTS_CHANCES.subList(2, 4))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.goldWeight.get(), new BackpackAddition(ModItems.GOLD_BACKPACK.get(), 2,
					HELMET_CHANCES.subList(1, 3), LEGGINGS_CHANCES.subList(1, 3), BOOTS_CHANCES.subList(1, 3))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.diamondWeight.get(), new BackpackAddition(ModItems.DIAMOND_BACKPACK.get(), 3,
					HELMET_CHANCES.subList(0, 2), LEGGINGS_CHANCES.subList(0, 2), BOOTS_CHANCES.subList(0, 2))),
			new WeightedElement<>(Config.SERVER.entityBackpackAdditions.netheriteWeight.get(), new BackpackAddition(ModItems.NETHERITE_BACKPACK.get(), 4,
					HELMET_CHANCES.subList(0, 1), LEGGINGS_CHANCES.subList(0, 1), BOOTS_CHANCES.subList(0, 1)))
	);

	private static final Map<Integer, List<WeightedElement<BackpackAddition>>> DIFFICULTY_BACKPACK_CHANCES = Map.of(
			0, BACKPACK_CHANCES,
			1, BACKPACK_CHANCES.subList(Config.SERVER.entityBackpackAdditions.minBackpackTierMidDifficulty.get(), 6),
			2, BACKPACK_CHANCES.subList(Config.SERVER.entityBackpackAdditions.minBackpackTierHighDifficulty.get(), 6)
	);

	static {
		VanillaDiscHandler.setDiscBlockListGetter(Config.SERVER.entityBackpackAdditions.discBlockList);
	}

	static void addBackpack(Monster monster, LevelAccessor level) {
		RandomSource rnd = level.getRandom();
		if (!Config.SERVER.entityBackpackAdditions.canWearBackpack(monster.getType())
				|| rnd.nextInt((int) (1 / Config.SERVER.entityBackpackAdditions.chance.get())) != 0 || (monster instanceof Raider raider && raider.getCurrentRaid() != null)) {
			return;
		}

		int difficultyIndex = 0;
		if (Config.SERVER.entityBackpackAdditions.localDifficultyEffectsBackpackSpawns.get()) {
			float localDifficulty = level.getCurrentDifficultyAt(monster.blockPosition()).getEffectiveDifficulty();
			//noinspection UnstableApiUsage
			difficultyIndex = Ints.constrainToRange((int) Math.floor(DIFFICULTY_BACKPACK_CHANCES.size() / MAX_LOCAL_DIFFICULTY * localDifficulty - 0.1f), 0, DIFFICULTY_BACKPACK_CHANCES.size() - 1);
		}

		RandHelper.getRandomWeightedElement(rnd, DIFFICULTY_BACKPACK_CHANCES.get(difficultyIndex)).ifPresent(backpackAddition -> {
			ItemStack backpack = new ItemStack(backpackAddition.getBackpackItem());
			int minDifficulty = backpackAddition.getMinDifficulty();
			equipBackpack(monster, backpack, minDifficulty, Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.playJukebox.get()) && rnd.nextInt(4) == 0, level, rnd);
			applyPotions(monster, Math.max(minDifficulty, rnd.nextInt(MAX_DIFFICULTY + 1)), minDifficulty, rnd);
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
					EnchantmentHelper.enchantItem(rnd, armorStack, enchantmentLevel, true);
				}
				monster.setItemSlot(slot, armorStack);
			}
		});
	}

	private static void equipBackpack(Monster monster, ItemStack backpack, int minDifficulty, boolean playMusicDisc, LevelAccessor level, RandomSource rnd) {
		getSpawnEgg(monster.getType()).ifPresent(egg -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(w -> {
					w.setColors(getPrimaryColor(egg), getSecondaryColor(egg));
					int partialRolls = rnd.nextInt(minDifficulty, minDifficulty + 3);
					if (partialRolls != 0) {
						setLoot(monster, w, level, (float) partialRolls / MAX_DIFFICULTY);
					}
					if (playMusicDisc) {
						w.getInventoryHandler(); //just to assign uuid and real upgrade handler
						if (w.getUpgradeHandler().getSlots() > 0) {
							monster.addTag(SPAWNED_WITH_JUKEBOX_UPGRADE);
							addJukeboxUpgradeAndRandomDisc(level.getRandom(), w, rnd);
						}
					}
				}));
		monster.setItemSlot(EquipmentSlot.CHEST, backpack);
		monster.setDropChance(EquipmentSlot.CHEST, 0);
	}

	private static void addJukeboxUpgradeAndRandomDisc(RandomSource random, IStorageWrapper w, RandomSource rnd) {
		boolean advancedJukebox = random.nextFloat() < 0.25;
		w.getUpgradeHandler().setStackInSlot(0, new ItemStack(advancedJukebox ? ModItems.ADVANCED_JUKEBOX_UPGRADE.get() : ModItems.JUKEBOX_UPGRADE.get()));
		Iterator<JukeboxUpgradeWrapper> it = w.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).iterator();
		if (it.hasNext()) {
			if (DiscHandlerRegistry.getHandlers().isEmpty()) {
				SophisticatedBackpacks.LOGGER.warn("No music discs found to add to jukebox upgrade, either there are none registered or they are all blocked");
				return;
			}

			JukeboxUpgradeWrapper wrapper = it.next();
			int numberOfDiscs = advancedJukebox ? random.nextInt(wrapper.getDiscInventory().getSlots() / 3) + 1 : 1;
			List<ItemStack> discsUsed = new ArrayList<>();
			for (int i = 0; i < numberOfDiscs * 2; i++) {
				final int slot = i;
				DiscHandlerRegistry.getRandomDisc(rnd).ifPresent(disc -> {
					if (discsUsed.stream().anyMatch(s -> ItemHandlerHelper.canItemStacksStack(s, disc))) {
						return;
					}
					wrapper.getDiscInventory().insertItem(slot, disc, false);
					discsUsed.add(disc);
				});
				if (discsUsed.size() >= numberOfDiscs) {
					break;
				}
			}
		}
	}

	private static void raiseHealth(Monster monster, int minDifficulty) {
		if (Boolean.FALSE.equals(Config.SERVER.entityBackpackAdditions.buffHealth.get())) {
			return;
		}
		AttributeInstance maxHealth = monster.getAttribute(Attributes.MAX_HEALTH);
		if (maxHealth != null) {
			double healthAddition = maxHealth.getBaseValue() * minDifficulty;
			if (healthAddition > 0.1D) {
				maxHealth.addPermanentModifier(new AttributeModifier("Backpack bearer health bonus", healthAddition, AttributeModifier.Operation.ADDITION));
			}
			monster.setHealth(monster.getMaxHealth());
		}
	}

	private static Optional<SpawnEggItem> getSpawnEgg(EntityType<?> entityType) {
		Map<EntityType<? extends Mob>, SpawnEggItem> eggs = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, null, "f_43201_");
		return eggs == null ? Optional.empty() : Optional.ofNullable(eggs.get(entityType));
	}

	private static int getPrimaryColor(SpawnEggItem egg) {
		Integer primaryColor = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, egg, "f_151200_");
		return primaryColor == null ? -1 : primaryColor;
	}

	private static int getSecondaryColor(SpawnEggItem egg) {
		Integer secondaryColor = ObfuscationReflectionHelper.getPrivateValue(SpawnEggItem.class, egg, "f_151201_");
		return secondaryColor == null ? -1 : secondaryColor;
	}

	private static final List<ApplicableEffect> APPLICABLE_EFFECTS = List.of(
			new ApplicableEffect(List.of(MobEffects.DAMAGE_RESISTANCE, MobEffects.REGENERATION), 1),
			new ApplicableEffect(MobEffects.FIRE_RESISTANCE),
			new ApplicableEffect(MobEffects.ABSORPTION),
			new ApplicableEffect(MobEffects.HEALTH_BOOST),
			new ApplicableEffect(MobEffects.MOVEMENT_SPEED),
			new ApplicableEffect(MobEffects.DAMAGE_BOOST));

	private static void setLoot(Monster monster, IBackpackWrapper backpackWrapper, LevelAccessor level, float lootFactor) {
		MinecraftServer server = level.getServer();
		if (server == null) {
			return;
		}

		if (Boolean.TRUE.equals(Config.SERVER.entityBackpackAdditions.addLoot.get())) {
			addLoot(monster, backpackWrapper, lootFactor);
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

	private static void addLoot(Monster monster, IBackpackWrapper backpackWrapper, float lootFactor) {
		Config.SERVER.entityBackpackAdditions.getLootTableName(monster.getType()).ifPresent(lootTableName -> backpackWrapper.setLoot(lootTableName, lootFactor));
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
		if (!event.getEntity().getTags().remove(SPAWNED_WITH_JUKEBOX_UPGRADE)) {
			return;
		}

		List<ItemStack> inventoryItems = new ArrayList<>();
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
			backpackWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
				wrapper.stop(event.getEntity());
				InventoryHelper.iterate(wrapper.getDiscInventory(), (slot, stack) -> {
					if (!stack.isEmpty()) {
						inventoryItems.add(wrapper.getDiscInventory().extractItem(slot, stack.getCount(), false));
					}
				});
			});
			InventoryHelper.iterate(backpackWrapper.getUpgradeHandler(), (slot, stack) -> {
				if (!stack.isEmpty()) {
					inventoryItems.add(backpackWrapper.getUpgradeHandler().extractItem(slot, stack.getCount(), false));
				}
			});
			backpackWrapper.removeContentsUuid();
		});

		ItemStackHandler inventory = new ItemStackHandler(inventoryItems.size());
		for (int i = 0; i < inventoryItems.size(); i++) {
			inventory.setStackInSlot(i, inventoryItems.get(i));
		}
		backpack.addTagElement(InventoryHandler.INVENTORY_TAG, inventory.serializeNBT());
	}

	private static boolean shouldDropBackpack(LivingDropsEvent event, Config.Server.EntityBackpackAdditionsConfig additionsConfig, LivingEntity mob, ItemStack backpack) {
		if (!(event.getSource().getEntity() instanceof Player)) {
			return false;
		}
		if (!Boolean.TRUE.equals(additionsConfig.dropToFakePlayers.get()) && event.getSource().getEntity() instanceof FakePlayer) {
			return false;
		}
		float lootingChanceMultiplier = dropChanceMultiplier.getOrDefault(backpack.getItem(), 1F);
		return mob.level().random.nextFloat() < (additionsConfig.backpackDropChance.get() + event.getLootingLevel() * additionsConfig.lootingChanceIncreasePerLevel.get()) * lootingChanceMultiplier;
	}

	public static void removeBeneficialEffects(Creeper creeper) {
		if (creeper.getTags().contains(SPAWNED_WITH_BACKPACK)) {
			creeper.getActiveEffects().removeIf(e -> e.getEffect().isBeneficial());
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
		stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(backpackWrapper -> backpackWrapper.getContentsUuid().ifPresent(uuid -> BackpackStorage.get().removeBackpackContents(uuid)));
	}

	public static void onLivingUpdate(LivingEvent.LivingTickEvent event) {
		LivingEntity entity = event.getEntity();
		if (!entity.getTags().contains(SPAWNED_WITH_JUKEBOX_UPGRADE)) {
			return;
		}
		entity.getItemBySlot(EquipmentSlot.CHEST).getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.ifPresent(backpackWrapper -> backpackWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
					if (wrapper.isPlaying()) {
						wrapper.tick(entity, entity.level(), entity.blockPosition());
					} else {
						wrapper.play(entity);
					}
				}));
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
		private final List<MobEffect> effects;

		private final int maxAmplifier;

		private ApplicableEffect(MobEffect effect) {
			this(List.of(effect), Integer.MAX_VALUE);
		}

		private ApplicableEffect(List<MobEffect> effects, int maxAmplifier) {
			this.effects = effects;
			this.maxAmplifier = maxAmplifier;
		}

		public MobEffect getRandomEffect(RandomSource random) {
			return effects.get(random.nextInt(effects.size()));
		}

		public int getMaxAmplifier() {
			return maxAmplifier;
		}
	}
}
