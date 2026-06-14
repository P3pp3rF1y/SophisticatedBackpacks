package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.core.Holder;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.effect.MobEffect;
import net.minecraft.world.effect.MobEffects;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.storage.loot.BuiltInLootTables;
import net.minecraft.world.level.storage.loot.LootTable;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.event.config.ModConfigEvent;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilteredUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeCountLimitConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeGroup;
import net.p3pp3rf1y.sophisticatedcore.upgrades.alchemy.AlchemyUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.AutoCookingUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.CookingUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.ICookingUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pump.PumpUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeConfig;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nullable;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("java:S1192")
//don't complain about repeated config names if two upgrades happen to have the same setting
public class Config {

	private static final String REGISTRY_NAME_MATCHER = "([a-z0-9_.-]+:[a-z0-9_/.-]+)";
	private static final String MAX_UPGRADES_MATCHER = "([a-z0-9_/.-]+\\|\\d+)";

	private Config() {
	}

	public static final Server SERVER;
	public static final ModConfigSpec SERVER_SPEC;

	public static final Common COMMON;
	public static final ModConfigSpec COMMON_SPEC;

	static {
		final Pair<Server, ModConfigSpec> serverSpec = new ModConfigSpec.Builder().configure(Server::new);
		SERVER_SPEC = serverSpec.getRight();
		SERVER = serverSpec.getLeft();

		final Pair<Common, ModConfigSpec> commonSpec = new ModConfigSpec.Builder().configure(Common::new);
		COMMON_SPEC = commonSpec.getRight();
		COMMON = commonSpec.getLeft();
	}

	public static class Server {
		public final DisallowedItems disallowedItems;
		public final NoInteractionBlocks noInteractionBlocks;
		public final NoConnectionBlocks noConnectionBlocks;
		public final BackpackConfig leatherBackpack;
		public final BackpackConfig copperBackpack;
		public final BackpackConfig ironBackpack;
		public final BackpackConfig goldBackpack;
		public final BackpackConfig diamondBackpack;
		public final BackpackConfig netheriteBackpack;
		public final CompactingUpgradeConfig compactingUpgrade;
		public final FilteredUpgradeConfig advancedCompactingUpgrade;
		public final FilteredUpgradeConfig depositUpgrade;
		public final FilteredUpgradeConfig advancedDepositUpgrade;
		public final FilteredUpgradeConfig feedingUpgrade;
		public final FilteredUpgradeConfig advancedFeedingUpgrade;
		public final FilteredUpgradeConfig filterUpgrade;
		public final FilteredUpgradeConfig advancedFilterUpgrade;
		public final MagnetUpgradeConfig magnetUpgrade;
		public final MagnetUpgradeConfig advancedMagnetUpgrade;
		public final FilteredUpgradeConfig pickupUpgrade;
		public final FilteredUpgradeConfig advancedPickupUpgrade;
		public final FilteredUpgradeConfig refillUpgrade;
		public final FilteredUpgradeConfig advancedRefillUpgrade;
		public final FilteredUpgradeConfig restockUpgrade;
		public final FilteredUpgradeConfig advancedRestockUpgrade;
		public final VoidUpgradeConfig voidUpgrade;
		public final VoidUpgradeConfig advancedVoidUpgrade;
		public final CookingUpgradeConfig smeltingUpgrade;
		public final CookingUpgradeConfig smokingUpgrade;
		public final CookingUpgradeConfig blastingUpgrade;
		public final AutoCookingUpgradeConfig autoSmeltingUpgrade;
		public final AutoCookingUpgradeConfig autoSmokingUpgrade;
		public final AutoCookingUpgradeConfig autoBlastingUpgrade;
		public final InceptionUpgradeConfig inceptionUpgrade;
		public final EntityBackpackAdditionsConfig entityBackpackAdditions;
		public final ModConfigSpec.BooleanValue itemFluidHandlerEnabled;
		public final ModConfigSpec.BooleanValue allowOpeningOtherPlayerBackpacks;
		public final ModConfigSpec.BooleanValue itemDisplayDisabled;
		public final ModConfigSpec.BooleanValue tickDedupeLogicDisabled;
		public final ModConfigSpec.BooleanValue canBePlacedInContainerItems;
		public final FilteredUpgradeConfig toolSwapperUpgrade;
		public final TankUpgradeConfig tankUpgrade;
		public final BatteryUpgradeConfig batteryUpgrade;
		public final StackUpgradeConfig stackUpgrade;
		public final PumpUpgradeConfig pumpUpgrade;
		public final XpPumpUpgradeConfig xpPumpUpgrade;
		public final JukeboxUpgradeConfig advancedJukeboxUpgrade;
		public final AlchemyUpgradeConfig alchemyUpgrade;
		public final AlchemyUpgradeConfig advancedAlchemyUpgrade;
		public final MobCatcherUpgradeConfig mobCatcherUpgrade;
		public final NerfsConfig nerfsConfig;
		public final MaxUgradesPerStorageConfig maxUpgradesPerStorage;

		public void initListeners(IEventBus modBus) {
			modBus.addListener(this::onConfigReload);
			modBus.addListener(this::onConfigLoad);
		}

		@SuppressWarnings("unused")
		//need the Event parameter for forge reflection to understand what event this listens to
		public void onConfigReload(ModConfigEvent.Reloading event) {
			clearCache();
		}

		@SuppressWarnings("unused")
		//need the Event parameter for forge reflection to understand what event this listens to
		public void onConfigLoad(ModConfigEvent.Loading event) {
			clearCache();
		}

		private void clearCache() {
			disallowedItems.initialized = false;
			stackUpgrade.clearNonStackableItems();
			compactingUpgrade.clearCache();
			maxUpgradesPerStorage.clearCache();
			mobCatcherUpgrade.clearCache();
			nerfsConfig.cachedEffect = null;
		}

		Server(ModConfigSpec.Builder builder) {
			builder.comment("Server Settings").push("server");

			disallowedItems = new DisallowedItems(builder);
			noInteractionBlocks = new NoInteractionBlocks(builder);
			noConnectionBlocks = new NoConnectionBlocks(builder);

			leatherBackpack = new BackpackConfig(builder, "Leather", 27, 1);
			copperBackpack = new BackpackConfig(builder, "Copper", 45, 1);
			ironBackpack = new BackpackConfig(builder, "Iron", 54, 2);
			goldBackpack = new BackpackConfig(builder, "Gold", 81, 3);
			diamondBackpack = new BackpackConfig(builder, "Diamond", 108, 5);
			netheriteBackpack = new BackpackConfig(builder, "Netherite", 120, 7);

			compactingUpgrade = new CompactingUpgradeConfig(builder, "Compacting Upgrade", "compactingUpgrade", 9, 3);
			advancedCompactingUpgrade = new FilteredUpgradeConfig(builder, "Advanced Compacting Upgrade", "advancedCompactingUpgrade", 16, 4);
			depositUpgrade = new FilteredUpgradeConfig(builder, "Deposit Upgrade", "depositUpgrade", 9, 3);
			advancedDepositUpgrade = new FilteredUpgradeConfig(builder, "Advanced Deposit Upgrade", "advancedDepositUpgrade", 16, 4);
			feedingUpgrade = new FilteredUpgradeConfig(builder, "Feeding Upgrade", "feedingUpgrade", 9, 3);
			advancedFeedingUpgrade = new FilteredUpgradeConfig(builder, "Advanced Feeding Upgrade", "advancedFeedingUpgrade", 16, 4);
			filterUpgrade = new FilteredUpgradeConfig(builder, "Filter Upgrade", "filterUpgrade", 9, 3);
			advancedFilterUpgrade = new FilteredUpgradeConfig(builder, "Advanced Filter Upgrade", "advancedFilterUpgrade", 16, 4);
			magnetUpgrade = new MagnetUpgradeConfig(builder, "Magnet Upgrade", "magnetUpgrade", 9, 3, 3);
			advancedMagnetUpgrade = new MagnetUpgradeConfig(builder, "Advanced Magnet Upgrade", "advancedMagnetUpgrade", 16, 4, 5);
			pickupUpgrade = new FilteredUpgradeConfig(builder, "Pickup Upgrade", "pickupUpgrade", 9, 3);
			advancedPickupUpgrade = new FilteredUpgradeConfig(builder, "Advanced Pickup Upgrade", "advancedPickupUpgrade", 16, 4);
			refillUpgrade = new FilteredUpgradeConfig(builder, "Refill Upgrade", "refillUpgrade", 6, 3);
			advancedRefillUpgrade = new FilteredUpgradeConfig(builder, "Advanced Refill Upgrade", "advancedRefillUpgrade", 12, 4);
			restockUpgrade = new FilteredUpgradeConfig(builder, "Restock Upgrade", "restockUpgrade", 9, 3);
			advancedRestockUpgrade = new FilteredUpgradeConfig(builder, "Advanced Restock Upgrade", "advancedRestockUpgrade", 16, 4);
			voidUpgrade = new VoidUpgradeConfig(builder, "Void Upgrade", "voidUpgrade", 9, 3);
			advancedVoidUpgrade = new VoidUpgradeConfig(builder, "Advanced Void Upgrade", "advancedVoidUpgrade", 16, 4);
			stackUpgrade = new StackUpgradeConfig(builder);
			smeltingUpgrade = CookingUpgradeConfig.getInstance(builder, "Smelting Upgrade", "smeltingUpgrade");
			smokingUpgrade = CookingUpgradeConfig.getInstance(builder, "Smoking Upgrade", "smokingUpgrade");
			blastingUpgrade = CookingUpgradeConfig.getInstance(builder, "Blasting Upgrade", "blastingUpgrade");
			autoSmeltingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Smelting Upgrade", "autoSmeltingUpgrade");
			autoSmokingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Smoking Upgrade", "autoSmokingUpgrade");
			autoBlastingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Blasting Upgrade", "autoBlastingUpgrade");
			inceptionUpgrade = new InceptionUpgradeConfig(builder);
			toolSwapperUpgrade = new FilteredUpgradeConfig(builder, "Tool Swapper Upgrade", "toolSwapperUpgrade", 8, 4);
			tankUpgrade = new TankUpgradeConfig(builder);
			batteryUpgrade = new BatteryUpgradeConfig(builder);
			pumpUpgrade = new PumpUpgradeConfig(builder);
			xpPumpUpgrade = new XpPumpUpgradeConfig(builder);
			advancedJukeboxUpgrade = new JukeboxUpgradeConfig(builder, "Advanced Jukebox Upgrade", "advancedJukeboxUpgrade", 12);
			alchemyUpgrade = new AlchemyUpgradeConfig(builder, "Alchemy Upgrade", "alchemyUpgrade", 4);
			advancedAlchemyUpgrade = new AlchemyUpgradeConfig(builder, "Advanced Alchemy Upgrade", "advancedAlchemyUpgrade", 8);
			mobCatcherUpgrade = new MobCatcherUpgradeConfig(builder);
			entityBackpackAdditions = new EntityBackpackAdditionsConfig(builder);
			nerfsConfig = new NerfsConfig(builder);
			maxUpgradesPerStorage = new MaxUgradesPerStorageConfig(builder,
					Map.of(
							StackUpgradeItem.UPGRADE_GROUP.name(), 3,
							ICookingUpgrade.UPGRADE_GROUP.name(), 1,
							JukeboxUpgradeItem.UPGRADE_GROUP.name(), 1
					)
			);

			itemFluidHandlerEnabled = builder.comment("Turns on/off item fluid handler of backpack in its item form. There are some dupe bugs caused by default fluid handling implementation that manifest when backpack is drained / filled in its item form in another mod's tank and the only way to prevent them is disallowing drain/fill in item form altogether").define("itemFluidHandlerEnabled", true);
			allowOpeningOtherPlayerBackpacks = builder.comment("Determines whether player can right click on backpack that another player is wearing to open it. If off will turn off that capability for everyone and remove related settings from backpack.").define("allowOpeningOtherPlayerBackpacks", true);
			itemDisplayDisabled = builder.comment("Allows disabling item display settings. Primarily in cases where custom backpack model doesn't support showing the item. (Requires game restart to take effect)").define("itemDisplayDisabled", false);
			tickDedupeLogicDisabled = builder.comment("Allows disabling logic that dedupes backpacks with the same UUID in players' inventory. This is here to allow turning off the logic just in case it would be causing performance issues.").define("tickDedupeLogicDisabled", false);
			canBePlacedInContainerItems = builder.comment("Determines if backpacks can be placed in container items (those that check for return value of canFitInsideContainerItems)").define("canBePlacedInContainerItems", false);

			builder.pop();
		}

		public static class NerfsConfig {
			public final ModConfigSpec.BooleanValue tooManyBackpacksSlowness;
			public final ModConfigSpec.IntValue maxNumberOfBackpacks;
			public final ModConfigSpec.DoubleValue slownessLevelsPerAdditionalBackpack;
			public final ModConfigSpec.BooleanValue onlyWornBackpackTriggersUpgrades;
			public final ModConfigSpec.ConfigValue<String> nerfEffect;

			@Nullable
			private Holder<MobEffect> cachedEffect = null;

			public NerfsConfig(ModConfigSpec.Builder builder) {
				builder.push("nerfs");
				tooManyBackpacksSlowness = builder.comment("Determines if too many backpacks in player's inventory cause slowness to the player").define("tooManyBackpacksSlowness", false);
				maxNumberOfBackpacks = builder.comment("Maximum number of backpacks in player's inventory that will not cause slowness").defineInRange("maxNumberOfBackpacks", 3, 1, 27);
				slownessLevelsPerAdditionalBackpack = builder.comment("Ratio of slowness levels per every backpack above the maximum number allowed. (number of backpacks above the max gets multiplied by this number and ceiled)").defineInRange("slownessLevelsPerAdditionalBackpack", 1, 0.1, 5);
				onlyWornBackpackTriggersUpgrades = builder.comment("Determines if active upgrades will only work in the backpack that's worn by the player. Active upgrades are for example magnet, pickup, cooking, feeding upgrades.").define("onlyWornBackpackTriggersUpgrades", false);
				nerfEffect = builder.comment("Effect that is applied to player when they have too many backpacks. Can be any effect including modded ones like overencumbered effect some mods have.").define("nerfEffect", "minecraft:slowness", s -> s instanceof String str && str.matches(REGISTRY_NAME_MATCHER));
				builder.pop();
			}

			public Holder<MobEffect> getEffect() {
				if (cachedEffect == null) {
					cachedEffect = BuiltInRegistries.MOB_EFFECT.getHolder(ResourceLocation.parse(nerfEffect.get())).<Holder<MobEffect>>map(h -> h).orElse(MobEffects.MOVEMENT_SLOWDOWN);
				}
				return cachedEffect;
			}
		}

		public static class EntityBackpackAdditionsConfig {
			private static final String ENTITY_LOOT_MATCHER = "([a-z0-9_.-]+:[a-z0-9_/.-]+)\\|(null|[a-z0-9_.-]+:[a-z0-9/_.-]+)";
			public final ModConfigSpec.DoubleValue chance;
			public final ModConfigSpec.BooleanValue addLoot;
			public final ModConfigSpec.BooleanValue buffWithPotionEffects;
			public final ModConfigSpec.BooleanValue buffHealth;
			public final ModConfigSpec.BooleanValue equipWithArmor;
			public final ModConfigSpec.BooleanValue playJukebox;
			public final ModConfigSpec.BooleanValue dropToFakePlayers;
			public final ModConfigSpec.DoubleValue backpackDropChance;
			public final ModConfigSpec.DoubleValue lootingChanceIncreasePerLevel;
			public final ModConfigSpec.IntValue leatherWeight;
			public final ModConfigSpec.IntValue copperWeight;
			public final ModConfigSpec.IntValue ironWeight;
			public final ModConfigSpec.IntValue goldWeight;
			public final ModConfigSpec.IntValue diamondWeight;
			public final ModConfigSpec.IntValue netheriteWeight;
			public final ModConfigSpec.IntValue minBackpackTierMidDifficulty;
			public final ModConfigSpec.IntValue minBackpackTierHighDifficulty;
			public final ModConfigSpec.BooleanValue localDifficultyEffectsBackpackSpawns;
			public final ModConfigSpec.ConfigValue<List<? extends String>> entityLootTableList;
			public final ModConfigSpec.ConfigValue<List<? extends String>> discBlockList;
			@Nullable
			private Map<EntityType<?>, ResourceLocation> entityLootTables = null;

			public EntityBackpackAdditionsConfig(ModConfigSpec.Builder builder) {
				builder.comment("Settings for Spawning Entities with Backpack").push("entityBackpackAdditions");
				chance = builder.comment("Chance of an entity spawning with Backpack").defineInRange("chance", 0.01, 0, 1);
				addLoot = builder.comment("Turns on/off addition of loot into backpacks").define("addLoot", true);
				buffWithPotionEffects = builder.comment("Turns on/off buffing the entity that wears backpack with potion effects. These are scaled based on how much loot is added.")
						.define("buffWithPotionEffects", true);
				buffHealth = builder.comment("Turns on/off buffing the entity that wears backpack with additional health. Health is scaled based on backpack tier the mob wears.")
						.define("buffHealth", true);
				equipWithArmor = builder.comment("Turns on/off equipping the entity that wears backpack with armor. What armor material and how enchanted it is scales based on backpack tier the mob wears.")
						.define("equipWithArmor", true);
				entityLootTableList = builder.comment("Map of entities that can spawn with backpack and related loot tables (if adding a loot is enabled) in format of \"EntityRegistryName|LootTableName\"")
						.defineList("entityLootTableList", this::getDefaultEntityLootTableList, mapping -> ((String) mapping).matches(ENTITY_LOOT_MATCHER));
				discBlockList = builder.comment("List of music discs that are not supposed to be played by entities")
						.defineList("discBlockList", this::getDefaultDiscBlockList, mapping -> ((String) mapping).matches(REGISTRY_NAME_MATCHER));
				playJukebox = builder.comment("Turns on/off a chance that the entity that wears backpack gets jukebox upgrade and plays a music disc.").define("playJukebox", true);
				dropToFakePlayers = builder.comment("Determines whether backpack drops to fake players if killed by them in addition to real ones that it always drops to").define("dropToFakePlayers", false);
				backpackDropChance = builder.comment("Chance of mob dropping backpack when killed by player").defineInRange("backpackDropChance", 0.5, 0, 1);
				lootingChanceIncreasePerLevel = builder.comment("Chance increase per looting level of mob dropping backpack").defineInRange("lootingChanceIncreasePerLevel", 0.15, 0, 0.3);
				leatherWeight = builder.comment("Weight of selecting a Leather Backpack when an entity spawns with a backpack").defineInRange("leatherWeight", 625, 0, 9999);
				copperWeight = builder.comment("Weight of selecting a Copper Backpack when an entity spawns with a backpack").defineInRange("copperWeight", 250, 0, 9999);
				ironWeight = builder.comment("Weight of selecting a Iron Backpack when an entity spawns with a backpack").defineInRange("ironWeight", 125, 0, 9999);
				goldWeight = builder.comment("Weight of selecting a Gold Backpack when an entity spawns with a backpack").defineInRange("goldWeight", 25, 0, 9999);
				diamondWeight = builder.comment("Weight of selecting a Diamond Backpack when an entity spawns with a backpack").defineInRange("diamondWeight", 5, 0, 9999);
				netheriteWeight = builder.comment("Weight of selecting a Netherite Backpack when an entity spawns with a backpack").defineInRange("netheriteWeight", 1, 0, 9999);
				minBackpackTierMidDifficulty = builder.comment("Minimum tier of backpack mobs are equipped with at mid local difficulty (above 1/3 of max, 0 is leather)")
						.defineInRange("minBackpackTierMidDifficulty", 1, 0, 6);
				minBackpackTierHighDifficulty = builder.comment("Minimum tier of backpack mobs are equipped with at high local difficulty (above 2/3 of max, 0 is leather)")
						.defineInRange("minBackpackTierHighDifficulty", 2, 0, 6);
				localDifficultyEffectsBackpackSpawns = builder.comment("If local difficulty is taken into consideration when determining the difficulty. If local difficulty is high enough then it will use difficulty settings above")
								.define("localDifficultyEffectsBackpackSpawns", true);
				builder.pop();
			}

			public Optional<ResourceLocation> getLootTableName(EntityType<?> entityType) {
				if (entityLootTables == null) {
					initEntityLootTables();
				}
				return Optional.ofNullable(entityLootTables.get(entityType));
			}

			public boolean canWearBackpack(EntityType<?> entityType) {
				if (entityLootTables == null) {
					initEntityLootTables();
				}
				return entityLootTables.containsKey(entityType);
			}

			private void initEntityLootTables() {
				entityLootTables = new HashMap<>();
				for (String mapping : entityLootTableList.get()) {
					String[] entityLoot = mapping.split("\\|");
					if (entityLoot.length < 2) {
						continue;
					}
					String entityRegistryName = entityLoot[0];
					String lootTableName = entityLoot[1];

					BuiltInRegistries.ENTITY_TYPE.getOptional(ResourceLocation.parse(entityRegistryName))
							.ifPresent(entityType -> entityLootTables.put(entityType, lootTableName.equals("null") ? null : ResourceLocation.parse(lootTableName)));
				}
			}

			private List<String> getDefaultDiscBlockList() {
				List<String> ret = new ArrayList<>();
				ret.add("botania:record_gaia_1");
				ret.add("botania:record_gaia_2");
				return ret;
			}

			private List<String> getDefaultEntityLootTableList() {
				return getDefaultEntityLootMapping().entrySet().stream().map(e -> BuiltInRegistries.ENTITY_TYPE.getKey(e.getKey()) + "|" + e.getValue().location()).collect(Collectors.toList());
			}

			private Map<EntityType<?>, ResourceKey<LootTable>> getDefaultEntityLootMapping() {
				Map<EntityType<?>, ResourceKey<LootTable>> mapping = new LinkedHashMap<>();
				mapping.put(EntityType.CREEPER, BuiltInLootTables.DESERT_PYRAMID);
				mapping.put(EntityType.DROWNED, BuiltInLootTables.SHIPWRECK_TREASURE);
				mapping.put(EntityType.ENDERMAN, BuiltInLootTables.END_CITY_TREASURE);
				mapping.put(EntityType.EVOKER, BuiltInLootTables.WOODLAND_MANSION);
				mapping.put(EntityType.HUSK, BuiltInLootTables.DESERT_PYRAMID);
				mapping.put(EntityType.PIGLIN, BuiltInLootTables.BASTION_BRIDGE);
				mapping.put(EntityType.PIGLIN_BRUTE, BuiltInLootTables.BASTION_TREASURE);
				mapping.put(EntityType.PILLAGER, BuiltInLootTables.PILLAGER_OUTPOST);
				mapping.put(EntityType.SKELETON, BuiltInLootTables.SIMPLE_DUNGEON);
				mapping.put(EntityType.STRAY, BuiltInLootTables.IGLOO_CHEST);
				mapping.put(EntityType.VEX, BuiltInLootTables.WOODLAND_MANSION);
				mapping.put(EntityType.VINDICATOR, BuiltInLootTables.WOODLAND_MANSION);
				mapping.put(EntityType.WITCH, BuiltInLootTables.BURIED_TREASURE);
				mapping.put(EntityType.WITHER_SKELETON, BuiltInLootTables.NETHER_BRIDGE);
				mapping.put(EntityType.ZOMBIE, BuiltInLootTables.SIMPLE_DUNGEON);
				mapping.put(EntityType.ZOMBIE_VILLAGER, BuiltInLootTables.VILLAGE_ARMORER);
				mapping.put(EntityType.ZOMBIFIED_PIGLIN, BuiltInLootTables.BASTION_OTHER);
				return mapping;
			}
		}

		public static class InceptionUpgradeConfig {
			public final ModConfigSpec.BooleanValue upgradesUseInventoriesOfBackpacksInBackpack;
			public final ModConfigSpec.BooleanValue upgradesInContainedBackpacksAreFunctional;

			public InceptionUpgradeConfig(ModConfigSpec.Builder builder) {
				builder.comment("Inception Upgrade Settings").push("inceptionUpgrade");
				upgradesUseInventoriesOfBackpacksInBackpack = builder.comment("Allows / Disallows backpack upgrades to work with inventories of Backpacks in the Backpack with Inception Upgrade")
						.define("upgradesUseInventoriesOfBackpacksInBackpack", true);
				upgradesInContainedBackpacksAreFunctional = builder.comment("Allows / Disallows upgrades to be functional even when they are in Backpacks in the inventory of Backpack with Inception Upgrade")
						.define("upgradesInContainedBackpacksAreFunctional", true);
				builder.pop();
			}
		}

		public static class BackpackConfig {
			public final ModConfigSpec.IntValue inventorySlotCount;
			public final ModConfigSpec.IntValue upgradeSlotCount;

			public BackpackConfig(ModConfigSpec.Builder builder, String backpackPrefix, int inventorySlotCountDefault, int upgradeSlotCountDefault) {
				builder.comment(backpackPrefix + " Backpack Settings").push(backpackPrefix.toLowerCase(Locale.ENGLISH) + "Backpack");
				inventorySlotCount = builder.comment("Number of inventory slots in the backpack").defineInRange("inventorySlotCount", inventorySlotCountDefault, 1, 144);
				upgradeSlotCount = builder.comment("Number of upgrade slots in the backpack").defineInRange("upgradeSlotCount", upgradeSlotCountDefault, 0, 10);
				builder.pop();
			}
		}

		public static class NoInteractionBlocks {
			private final ModConfigSpec.ConfigValue<List<? extends String>> noInteractionBlocksList;
			private boolean initialized = false;
			private Set<Block> noInteractionBlocksSet = null;

			NoInteractionBlocks(ModConfigSpec.Builder builder) {
				noInteractionBlocksList = builder.comment("List of blocks that inventory interaction upgrades can't interact with - e.g. \"minecraft:shulker_box\"")
						.defineListAllowEmpty("noInteractionBlocks", ArrayList::new, () -> "minecraft:shulker_box", mapping -> mapping instanceof String str && str.matches(REGISTRY_NAME_MATCHER));
			}

			public boolean isBlockInteractionDisallowed(Block block) {
				if (!SERVER_SPEC.isLoaded()) {
					return true;
				}
				if (!initialized) {
					loadDisallowedSet();
				}
				return noInteractionBlocksSet.contains(block);
			}

			private void loadDisallowedSet() {
				initialized = true;
				noInteractionBlocksSet = new HashSet<>();

				for (String disallowedItemName : noInteractionBlocksList.get()) {
					ResourceLocation registryName = ResourceLocation.parse(disallowedItemName);
					if (BuiltInRegistries.BLOCK.containsKey(registryName)) {
						noInteractionBlocksSet.add(BuiltInRegistries.BLOCK.get(registryName));
					}
				}
			}
		}

		public static class NoConnectionBlocks {
			private final ModConfigSpec.ConfigValue<List<? extends String>> noConnectionBlocksList;
			private final ModConfigSpec.BooleanValue allBlockConnectionsDisallowed;
			private boolean initialized = false;
			private Set<Block> noConnnectionBlocksSet = null;

			NoConnectionBlocks(ModConfigSpec.Builder builder) {
				noConnectionBlocksList = builder.comment("List of blocks that are not allowed to connect to backpacks - e.g. \"refinedstorage:external_storage\"")
						.defineList("noConnectionBlocks", new ArrayList<>(), mapping -> ((String) mapping).matches(REGISTRY_NAME_MATCHER));
				allBlockConnectionsDisallowed = builder.comment("If true, disallows all blocks from connecting to backpacks").define("allBlockConnectionsDisallowed", false);
			}

			public boolean isBlockConnectionDisallowed(Block block) {
				if (!SERVER_SPEC.isLoaded()) {
					return true;
				}
				if (allBlockConnectionsDisallowed.get()) {
					return true;
				}
				if (!initialized) {
					loadDisallowedSet();
				}
				return noConnnectionBlocksSet.contains(block);
			}

			private void loadDisallowedSet() {
				initialized = true;
				noConnnectionBlocksSet = new HashSet<>();

				for (String disallowedItemName : noConnectionBlocksList.get()) {
					ResourceLocation registryName = ResourceLocation.parse(disallowedItemName);
					if (BuiltInRegistries.BLOCK.containsKey(registryName)) {
						noConnnectionBlocksSet.add(BuiltInRegistries.BLOCK.get(registryName));
					}
				}
			}
		}

		public static class DisallowedItems {
			private final ModConfigSpec.BooleanValue containerItemsDisallowed;
			private final ModConfigSpec.ConfigValue<List<? extends String>> disallowedItemsList;
			private boolean initialized = false;
			private Set<Item> disallowedItemsSet = null;

			DisallowedItems(ModConfigSpec.Builder builder) {
				disallowedItemsList = builder.comment("List of items that are not allowed to be put in backpacks - e.g. \"minecraft:shulker_box\"")
						.defineListAllowEmpty("disallowedItems", ArrayList::new, () -> "minecraft:shulker_box", mapping -> mapping instanceof String str && str.matches(REGISTRY_NAME_MATCHER));
				containerItemsDisallowed = builder.comment("Determines if container items (those that override canFitInsideContainerItems to false) are able to fit in backpacks")
						.define("containerItemsDisallowed", false);
			}

			public boolean isItemDisallowed(Item item) {
				if (!SERVER_SPEC.isLoaded()) {
					return true;
				}

				if (!initialized) {
					loadDisallowedSet();
				}

				if (Boolean.TRUE.equals(containerItemsDisallowed.get()) && !(item instanceof BackpackItem) && !item.canFitInsideContainerItems()) {
					return true;
				}

				return disallowedItemsSet.contains(item);
			}

			private void loadDisallowedSet() {
				initialized = true;
				disallowedItemsSet = new HashSet<>();

				for (String disallowedItemName : disallowedItemsList.get()) {
					ResourceLocation registryName = ResourceLocation.parse(disallowedItemName);
					BuiltInRegistries.ITEM.getOptional(registryName).ifPresent(disallowedItemsSet::add);
				}
			}
		}

		public static class MaxUgradesPerStorageConfig implements IUpgradeCountLimitConfig {
			private final ModConfigSpec.ConfigValue<List<? extends String>> maxUpgradesPerStorageList;

			@Nullable
			private Map<String, Integer> maxUpgradesPerStorage = null;

			protected MaxUgradesPerStorageConfig(ModConfigSpec.Builder builder, Map<String, Integer> defaultUpgradesPerStorage) {
				maxUpgradesPerStorageList = builder.comment("Maximum number of upgrades of type per backpack in format of \"UpgradeRegistryName[or UpgradeGroup]|MaxNumber\"")
						.defineList("maxUpgradesPerStorage", convertToList(defaultUpgradesPerStorage), mapping -> ((String) mapping).matches(MAX_UPGRADES_MATCHER));
			}

			private List<String> convertToList(Map<String, Integer> defaultUpgradesPerStorage) {
				return defaultUpgradesPerStorage.entrySet().stream().map(e -> e.getKey() + "|" + e.getValue()).collect(Collectors.toList());
			}

			public void clearCache() {
				maxUpgradesPerStorage = null;
			}

			@Override
			public int getMaxUpgradesPerStorage(String storageType, @Nullable ResourceLocation upgradeRegistryName) {
				if (maxUpgradesPerStorage == null) {
					initMaxUpgradesPerStorage();
				}
				if (upgradeRegistryName == null) {
					return Integer.MAX_VALUE;
				}

				return maxUpgradesPerStorage.getOrDefault(upgradeRegistryName.getPath(), Integer.MAX_VALUE);
			}

			private void initMaxUpgradesPerStorage() {
				maxUpgradesPerStorage = new HashMap<>();
				for (String mapping : maxUpgradesPerStorageList.get()) {
					String[] upgradeMax = mapping.split("\\|");
					if (upgradeMax.length < 2) {
						continue;
					}
					String name = upgradeMax[0];
					int max = Integer.parseInt(upgradeMax[1]);
					maxUpgradesPerStorage.put(name, max);
				}
			}

			@Override
			public int getMaxUpgradesInGroupPerStorage(String storageType, UpgradeGroup upgradeGroup) {
				if (maxUpgradesPerStorage == null) {
					initMaxUpgradesPerStorage();
				}
				return maxUpgradesPerStorage.getOrDefault(upgradeGroup.name(), Integer.MAX_VALUE);
			}
		}
	}

	public static class Common {
		public final ModConfigSpec.BooleanValue chestLootEnabled;

		Common(ModConfigSpec.Builder builder) {
			builder.comment("Common Settings").push("common");

			chestLootEnabled = builder.comment("Turns on/off loot added to various vanilla chest loot tables").define("chestLootEnabled", true);
			builder.pop();
		}
	}
}
