package net.p3pp3rf1y.sophisticatedbackpacks.common;

import com.google.common.primitives.Ints;
import net.minecraft.core.Holder;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.MinecraftServer;
import net.minecraft.tags.EnchantmentTags;
import net.minecraft.util.RandomSource;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffectInstance;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
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
import net.minecraft.world.item.SpawnEggItem;
import net.minecraft.world.item.component.ItemContainerContents;
import net.minecraft.world.item.enchantment.EnchantmentHelper;
import net.minecraft.world.item.enchantment.Enchantments;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.LevelAccessor;
import net.neoforged.neoforge.common.util.FakePlayer;
import net.neoforged.neoforge.event.entity.living.LivingConversionEvent;
import net.neoforged.neoforge.event.entity.living.LivingDropsEvent;
import net.neoforged.neoforge.event.tick.EntityTickEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
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
	private static final net.minecraft.resources.ResourceLocation BACKPACK_BEARER_HEALTH_BONUS = SophisticatedBackpacks.getRL("backpack_bearer_health_bonus");
	private static final String ENTITY_DATA_TAG = SophisticatedBackpacks.MOD_ID;
	private static final String ENTITY_DATA_SPAWNED_WITH_BACKPACK = "spawnedWithBackpack";
	private static final String ENTITY_DATA_SPAWNED_WITH_JUKEBOX_UPGRADE = "spawnedWithJukeboxUpgrade";
	private static final String ENTITY_DATA_PENDING_BACKPACK_ADDITION = "pendingBackpackAddition";
	private static final String ENTITY_DATA_CONVERTING = "converting";

	private EntityBackpackAdditionHandler() {
	}

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

	static {
		VanillaDiscHandler.setDiscBlockListGetter(Config.SERVER.entityBackpackAdditions.discBlockList);
	}

	static void handleBackpackAdditionOnSpawn(Monster monster, LevelAccessor level) {
		RandomSource rnd = level.getRandom();
		if (!shouldAddBackpack(monster, rnd)) {
			return;
		}

		if (shouldDeferBackpackAddition(level)) {
			setPendingBackpackAddition(monster, true);
			return;
		}

		addBackpack(monster, level, rnd);
	}

	private static boolean shouldAddBackpack(Monster monster, RandomSource rnd) {
		return Config.SERVER.entityBackpackAdditions.canWearBackpack(monster.getType())
				&& rnd.nextInt((int) (1 / Config.SERVER.entityBackpackAdditions.chance.get())) == 0
				&& (!(monster instanceof Raider raider) || raider.getCurrentRaid() == null);
	}

	private static boolean shouldDeferBackpackAddition(LevelAccessor level) {
		MinecraftServer server = level.getServer();
		return server == null || !server.isSameThread();
	}

	private static void addBackpack(Monster monster, LevelAccessor level, RandomSource rnd) {
		int difficultyIndex = 0;
		if (Config.SERVER.entityBackpackAdditions.localDifficultyEffectsBackpackSpawns.getAsBoolean()) {
			float localDifficulty = level.getCurrentDifficultyAt(monster.blockPosition()).getEffectiveDifficulty();
			difficultyIndex = Ints.constrainToRange((int) Math.floor(DIFFICULTY_BACKPACK_CHANCES.size() / MAX_LOCAL_DIFFICULTY * localDifficulty - 0.1f), 0, DIFFICULTY_BACKPACK_CHANCES.size() - 1);
		}

		RandHelper.getRandomWeightedElement(rnd, DIFFICULTY_BACKPACK_CHANCES.get(difficultyIndex)).ifPresent(backpackAddition -> {
			ItemStack backpack = new ItemStack(backpackAddition.getBackpackItem());
			int minDifficulty = backpackAddition.getMinDifficulty();
			equipBackpack(monster, backpack, minDifficulty, Config.SERVER.entityBackpackAdditions.playJukebox.get() && rnd.nextInt(4) == 0, level, rnd);
			applyPotions(monster, Math.max(minDifficulty, rnd.nextInt(MAX_DIFFICULTY + 1)), minDifficulty, rnd);
			raiseHealth(monster, minDifficulty);
			if (Config.SERVER.entityBackpackAdditions.equipWithArmor.get()) {
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getHelmetChances(), EquipmentSlot.HEAD, level);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getLeggingsChances(), EquipmentSlot.LEGS, level);
				equipArmorPiece(monster, rnd, minDifficulty, backpackAddition.getBootsChances(), EquipmentSlot.FEET, level);
			}
			setSpawnedBackpack(monster, true);
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
							level.registryAccess().registryOrThrow(Registries.ENCHANTMENT).getTag(EnchantmentTags.ON_MOB_SPAWN_EQUIPMENT));
				}
				monster.setItemSlot(slot, armorStack);
			}
		});
	}

	private static void equipBackpack(Monster monster, ItemStack backpack, int minDifficulty, boolean playMusicDisc, LevelAccessor level, RandomSource rnd) {
		setSpawnedBackpack(monster, true);
		getSpawnEgg(monster.getType()).ifPresent(egg -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(backpack);
			wrapper.setColors(egg.getColor(0) | 0xFF_000000, egg.getColor(1) | 0xFF_000000);
			int partialRolls = rnd.nextInt(minDifficulty, minDifficulty + 3);
			if (partialRolls != 0) {
				setLoot(monster, wrapper, level, (float) partialRolls / MAX_DIFFICULTY);
			}
			if (playMusicDisc) {
				wrapper.getInventoryHandler(); //just to assign uuid and real upgrade handler
				if (wrapper.getUpgradeHandler().getSlots() > 0) {
					setSpawnedJukeboxUpgrade(monster, true);
					addJukeboxUpgradeAndRandomDisc(level.getRandom(), wrapper, rnd);
				}
			}
		});
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
					if (discsUsed.stream().anyMatch(s -> ItemStack.isSameItemSameComponents(s, disc))) {
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
		if (!Config.SERVER.entityBackpackAdditions.buffHealth.get()) {
			return;
		}
		AttributeInstance maxHealth = monster.getAttribute(Attributes.MAX_HEALTH);
		if (maxHealth != null) {
			double healthAddition = maxHealth.getBaseValue() * minDifficulty;
			if (healthAddition > 0.1D) {
				maxHealth.addPermanentModifier(new AttributeModifier(BACKPACK_BEARER_HEALTH_BONUS, healthAddition, AttributeModifier.Operation.ADD_VALUE));
			}
			monster.setHealth(monster.getMaxHealth());
		}
	}

	private static Optional<SpawnEggItem> getSpawnEgg(EntityType<?> entityType) {
		return Optional.ofNullable(SpawnEggItem.BY_ID.get(entityType));
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

		if (Config.SERVER.entityBackpackAdditions.addLoot.get()) {
			addLoot(monster, backpackWrapper, lootFactor);
		}
	}

	private static void applyPotions(Monster monster, int difficulty, int minDifficulty, RandomSource rnd) {
		if (Config.SERVER.entityBackpackAdditions.buffWithPotionEffects.get()) {
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
		if (hasSpawnedBackpack(event.getEntity())) {
			LivingEntity mob = event.getEntity();
			ItemStack backpack = mob.getItemBySlot(EquipmentSlot.CHEST);
			Config.Server.EntityBackpackAdditionsConfig additionsConfig = Config.SERVER.entityBackpackAdditions;
			if (shouldDropBackpack(event, additionsConfig, mob, backpack)) {
				putJukeboxItemsInContainerAndRemoveStorageUuid(event, backpack);

				ItemEntity backpackEntity = new ItemEntity(mob.level(), mob.getX(), mob.getY(), mob.getZ(), backpack);
				event.getDrops().add(backpackEntity);
				mob.setItemSlot(EquipmentSlot.CHEST, ItemStack.EMPTY);
				clearSpawnedBackpackData(mob);
			} else {
				clearSpawnedBackpackData(mob);
				removeContentsUuid(backpack);
			}
		}
	}

	static void handleLivingConversionPre(LivingConversionEvent.Pre event) {
		if (hasSpawnedBackpack(event.getEntity())) {
			getOrCreateBackpackEntityData(event.getEntity()).putBoolean(ENTITY_DATA_CONVERTING, true);
		}
	}

	static void handleLivingConversion(LivingConversionEvent.Post event) {
		LivingEntity entity = event.getEntity();
		LivingEntity outcome = event.getOutcome();
		ItemStack backpack = outcome.getItemBySlot(EquipmentSlot.CHEST);
		if (!hasSpawnedBackpack(entity) || !(backpack.getItem() instanceof BackpackItem)) {
			return;
		}

		IBackpackWrapper outcomeWrapper = BackpackWrapper.fromStack(backpack);
		outcomeWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> wrapper.stop(entity));

		copySpawnedBackpackData(entity, outcome);

		transferHealthBonus(entity, outcome);
	}

	private static void transferHealthBonus(LivingEntity entity, LivingEntity outcome) {
		AttributeInstance sourceMaxHealth = entity.getAttribute(Attributes.MAX_HEALTH);
		AttributeInstance outcomeMaxHealth = outcome.getAttribute(Attributes.MAX_HEALTH);
		if (sourceMaxHealth == null || outcomeMaxHealth == null) {
			return;
		}

		AttributeModifier healthModifier = sourceMaxHealth.getModifier(BACKPACK_BEARER_HEALTH_BONUS);
		if (healthModifier == null || outcomeMaxHealth.getModifier(BACKPACK_BEARER_HEALTH_BONUS) != null) {
			return;
		}

		float healthRatio = entity.getHealth() / entity.getMaxHealth();
		outcomeMaxHealth.addPermanentModifier(healthModifier);
		outcome.setHealth(Math.min(outcome.getMaxHealth(), outcome.getMaxHealth() * healthRatio));
	}

	private static void putJukeboxItemsInContainerAndRemoveStorageUuid(LivingDropsEvent event, ItemStack backpack) {
		if (hasSpawnedJukeboxUpgrade(event.getEntity()) && backpack.getItem() instanceof BackpackItem) {
			List<ItemStack> inventoryItems = new ArrayList<>();
			IBackpackWrapper backpackwrapper = BackpackWrapper.fromStack(backpack);
			backpackwrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
				wrapper.stop(event.getEntity());
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
		if (!additionsConfig.dropToFakePlayers.get() && event.getSource().getEntity() instanceof FakePlayer) {
			return false;
		}
		float lootingChanceMultiplier = dropChanceMultiplier.getOrDefault(backpack.getItem(), 1F);
		int lootingLevel = player.level().registryAccess().registry(Registries.ENCHANTMENT).map(registry -> player.getMainHandItem().getEnchantmentLevel(registry.getHolderOrThrow(Enchantments.LOOTING))).orElse(0);
		return mob.level().random.nextFloat() < (additionsConfig.backpackDropChance.get() + lootingLevel * additionsConfig.lootingChanceIncreasePerLevel.get()) * lootingChanceMultiplier;
	}

	public static void removeBeneficialEffects(Creeper creeper) {
		if (hasSpawnedBackpack(creeper)) {
			creeper.getActiveEffects().removeIf(e -> e.getEffect().value().isBeneficial());
		}
	}

	public static void removeBackpackUuid(Monster entity, Level level) {
		if (level.isClientSide() || !hasSpawnedBackpack(entity)) {
			return;
		}
		if (isConverting(entity)) {
			return;
		}

		ItemStack stack = entity.getItemBySlot(EquipmentSlot.CHEST);
		clearSpawnedBackpackData(entity);
		removeContentsUuid(stack);
	}

	private static void removeContentsUuid(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			return;
		}
		BackpackWrapper.fromStack(stack).getContentsUuid().ifPresent(uuid -> BackpackStorage.get().removeBackpackContents(uuid));
	}

	public static void onLivingUpdate(EntityTickEvent.Post event) {
		Entity entity = event.getEntity();
		if (entity.level().isClientSide() || entity instanceof Player || !(entity instanceof LivingEntity livingEntity)) {
			return;
		}
		if (livingEntity instanceof Monster monster && hasPendingBackpackAddition(monster)) {
			setPendingBackpackAddition(monster, false);
			if (monster.getItemBySlot(EquipmentSlot.CHEST).isEmpty()) {
				addBackpack(monster, entity.level(), entity.level().getRandom());
			}
		}
		if (!hasSpawnedJukeboxUpgrade(livingEntity)) {
			return;
		}
		ItemStack backpack = livingEntity.getItemBySlot(EquipmentSlot.CHEST);
		if (!(backpack.getItem() instanceof BackpackItem)) {
			clearSpawnedBackpackData(livingEntity);
			return;
		}

		IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(backpack);
		backpackWrapper.getUpgradeHandler().getTypeWrappers(JukeboxUpgradeItem.TYPE).forEach(wrapper -> {
			if (wrapper.isPlaying()) {
				wrapper.tick(livingEntity, entity.level(), entity.blockPosition());
			} else {
				wrapper.play(livingEntity);
			}
		});
	}

	private static boolean hasSpawnedBackpack(LivingEntity entity) {
		return getBackpackEntityData(entity).getBoolean(ENTITY_DATA_SPAWNED_WITH_BACKPACK);
	}

	private static boolean hasSpawnedJukeboxUpgrade(LivingEntity entity) {
		return getBackpackEntityData(entity).getBoolean(ENTITY_DATA_SPAWNED_WITH_JUKEBOX_UPGRADE);
	}

	private static void setSpawnedBackpack(LivingEntity entity, boolean spawnedWithBackpack) {
		getOrCreateBackpackEntityData(entity).putBoolean(ENTITY_DATA_SPAWNED_WITH_BACKPACK, spawnedWithBackpack);
	}

	private static void setSpawnedJukeboxUpgrade(LivingEntity entity, boolean spawnedWithJukeboxUpgrade) {
		getOrCreateBackpackEntityData(entity).putBoolean(ENTITY_DATA_SPAWNED_WITH_JUKEBOX_UPGRADE, spawnedWithJukeboxUpgrade);
	}

	private static boolean hasPendingBackpackAddition(LivingEntity entity) {
		return getBackpackEntityData(entity).getBoolean(ENTITY_DATA_PENDING_BACKPACK_ADDITION);
	}

	private static void setPendingBackpackAddition(LivingEntity entity, boolean pendingBackpackAddition) {
		if (pendingBackpackAddition) {
			getOrCreateBackpackEntityData(entity).putBoolean(ENTITY_DATA_PENDING_BACKPACK_ADDITION, true);
		} else {
			CompoundTag backpackEntityData = getBackpackEntityData(entity);
			backpackEntityData.remove(ENTITY_DATA_PENDING_BACKPACK_ADDITION);
			if (backpackEntityData.isEmpty()) {
				entity.getPersistentData().remove(ENTITY_DATA_TAG);
			}
		}
	}

	private static CompoundTag getBackpackEntityData(LivingEntity entity) {
		return entity.getPersistentData().getCompound(ENTITY_DATA_TAG);
	}

	private static CompoundTag getOrCreateBackpackEntityData(LivingEntity entity) {
		CompoundTag persistentData = entity.getPersistentData();
		if (!persistentData.contains(ENTITY_DATA_TAG, CompoundTag.TAG_COMPOUND)) {
			persistentData.put(ENTITY_DATA_TAG, new CompoundTag());
		}
		return persistentData.getCompound(ENTITY_DATA_TAG);
	}

	private static void copySpawnedBackpackData(LivingEntity entity, LivingEntity outcome) {
		CompoundTag sourceData = entity.getPersistentData().getCompound(ENTITY_DATA_TAG);
		if (sourceData.isEmpty()) {
			setSpawnedBackpack(outcome, true);
			return;
		}

		setSpawnedBackpack(outcome, sourceData.getBoolean(ENTITY_DATA_SPAWNED_WITH_BACKPACK));
		if (hasSpawnedJukeboxUpgrade(entity)) {
			setSpawnedJukeboxUpgrade(outcome, true);
		}
	}

	private static boolean isConverting(LivingEntity entity) {
		return getBackpackEntityData(entity).getBoolean(ENTITY_DATA_CONVERTING);
	}

	private static void clearSpawnedBackpackData(LivingEntity entity) {
		entity.getPersistentData().remove(ENTITY_DATA_TAG);
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
