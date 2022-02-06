package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.entity.EntityType;
import net.minecraft.item.Item;
import net.minecraft.loot.LootTables;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.config.ModConfig;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SortButtonsPosition;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

@SuppressWarnings("java:S1192") //don't complain about repeated config names if two upgrades happen to have the same setting
public class Config {
	private static final String SETTINGS = " Settings";

	private Config() {}

	public static final Client CLIENT;
	public static final ForgeConfigSpec CLIENT_SPEC;
	public static final Common COMMON;
	public static final ForgeConfigSpec COMMON_SPEC;

	static {
		final Pair<Client, ForgeConfigSpec> clientSpec = new ForgeConfigSpec.Builder().configure(Client::new);
		CLIENT_SPEC = clientSpec.getRight();
		CLIENT = clientSpec.getLeft();

		final Pair<Common, ForgeConfigSpec> commonSpec = new ForgeConfigSpec.Builder().configure(Common::new);
		COMMON_SPEC = commonSpec.getRight();
		COMMON = commonSpec.getLeft();
	}

	public static class Client {
		public final ForgeConfigSpec.EnumValue<SortButtonsPosition> sortButtonsPosition;
		public final ForgeConfigSpec.BooleanValue playButtonSound;

		Client(ForgeConfigSpec.Builder builder) {
			builder.comment("Client Settings").push("client");
			sortButtonsPosition = builder.comment("Positions where sort buttons can display to help with conflicts with controls from other mods").defineEnum("sortButtonsPosition", SortButtonsPosition.TITLE_LINE_RIGHT);
			playButtonSound = builder.comment("Whether click sound should play when custom buttons are clicked in backpack gui").define("playButtonSound", true);
			builder.pop();
		}
	}

	public static class Common {
		public final EnabledItems enabledItems;
		public final DisallowedItems disallowedItems;
		public final BackpackConfig leatherBackpack;
		public final BackpackConfig ironBackpack;
		public final BackpackConfig goldBackpack;
		public final BackpackConfig diamondBackpack;
		public final BackpackConfig netheriteBackpack;
		public final FilteredUpgradeConfig compactingUpgrade;
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
		public final FilteredUpgradeConfig restockUpgrade;
		public final FilteredUpgradeConfig advancedRestockUpgrade;
		public final FilteredUpgradeConfig voidUpgrade;
		public final FilteredUpgradeConfig advancedVoidUpgrade;
		public final CookingUpgradeConfig smeltingUpgrade;
		public final CookingUpgradeConfig smokingUpgrade;
		public final CookingUpgradeConfig blastingUpgrade;
		public final AutoCookingUpgradeConfig autoSmeltingUpgrade;
		public final AutoCookingUpgradeConfig autoSmokingUpgrade;
		public final AutoCookingUpgradeConfig autoBlastingUpgrade;
		public final InceptionUpgradeConfig inceptionUpgrade;
		public final EntityBackpackAdditionsConfig entityBackpackAdditions;
		public final ForgeConfigSpec.BooleanValue chestLootEnabled;
		public final ToolSwapperUpgradeConfig toolSwapperUpgrade;
		public final TankUpgradeConfig tankUpgrade;
		public final BatteryUpgradeConfig batteryUpgrade;
		public final StackUpgradeConfig stackUpgrade;
		public final PumpUpgradeConfig pumpUpgrade;
		public final XpPumpUpgradeConfig xpPumpUpgrade;

		@SuppressWarnings("unused") //need the Event parameter for forge reflection to understand what event this listens to
		public void onConfigReload(ModConfig.Reloading event) {
			enabledItems.enabledMap.clear();
			disallowedItems.setInitialized = false;
			stackUpgrade.nonStackableItems = null;
		}

		Common(ForgeConfigSpec.Builder builder) {
			builder.comment("Common Settings").push("common");

			enabledItems = new EnabledItems(builder);
			disallowedItems = new DisallowedItems(builder);

			leatherBackpack = new BackpackConfig(builder, "Leather", 27, 1);
			ironBackpack = new BackpackConfig(builder, "Iron", 54, 2);
			goldBackpack = new BackpackConfig(builder, "Gold", 81, 3);
			diamondBackpack = new BackpackConfig(builder, "Diamond", 108, 5);
			netheriteBackpack = new BackpackConfig(builder, "Netherite", 120, 7);

			compactingUpgrade = new FilteredUpgradeConfig(builder, "Compacting Upgrade", "compactingUpgrade", 9, 3);
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
			restockUpgrade = new FilteredUpgradeConfig(builder, "Restock Upgrade", "restockUpgrade", 9, 3);
			advancedRestockUpgrade = new FilteredUpgradeConfig(builder, "Advanced Restock Upgrade", "advancedRestockUpgrade", 16, 4);
			voidUpgrade = new FilteredUpgradeConfig(builder, "Void Upgrade", "voidUpgrade", 9, 3);
			advancedVoidUpgrade = new FilteredUpgradeConfig(builder, "Advanced Void Upgrade", "advancedVoidUpgrade", 16, 4);
			stackUpgrade = new StackUpgradeConfig(builder);
			smeltingUpgrade = CookingUpgradeConfig.getInstance(builder, "Smelting Upgrade", "smeltingUpgrade");
			smokingUpgrade = CookingUpgradeConfig.getInstance(builder, "Smoking Upgrade", "smokingUpgrade");
			blastingUpgrade = CookingUpgradeConfig.getInstance(builder, "Blasting Upgrade", "blastingUpgrade");
			autoSmeltingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Smelting Upgrade", "autoSmeltingUpgrade");
			autoSmokingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Smoking Upgrade", "autoSmokingUpgrade");
			autoBlastingUpgrade = new AutoCookingUpgradeConfig(builder, "Auto-Blasting Upgrade", "autoBlastingUpgrade");
			inceptionUpgrade = new InceptionUpgradeConfig(builder);
			toolSwapperUpgrade = new ToolSwapperUpgradeConfig(builder);
			tankUpgrade = new TankUpgradeConfig(builder);
			batteryUpgrade = new BatteryUpgradeConfig(builder);
			pumpUpgrade = new PumpUpgradeConfig(builder);
			xpPumpUpgrade = new XpPumpUpgradeConfig(builder);
			entityBackpackAdditions = new EntityBackpackAdditionsConfig(builder);

			chestLootEnabled = builder.comment("Turns on/off loot added to various vanilla chest loot tables").define("chestLootEnabled", true);

			builder.pop();
		}

		public static class XpPumpUpgradeConfig {
			public final ForgeConfigSpec.IntValue maxXpPointsPerMending;
			public final ForgeConfigSpec.BooleanValue mendingOn;

			public XpPumpUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Xp Pump Upgrade" + SETTINGS).push("xpPumpUpgrade");
				mendingOn = builder.comment("Whether xp pump can mend items with mending. Set false here to turn off the feature altogether.").define("mendingOn", true);
				maxXpPointsPerMending = builder.comment("How many experience points at a maximum would be used to mend an item per operation (every 5 ticks and 1 xp point usually translates to 2 damage repaired).").defineInRange("maxXpPointsPerMending", 5, 1, 20);
				builder.pop();
			}
		}

		public static class PumpUpgradeConfig {
			public final ForgeConfigSpec.IntValue maxInputOutput;
			public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
			public final ForgeConfigSpec.IntValue filterSlots;

			public PumpUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Pump Upgrade" + SETTINGS).push("pumpUpgrade");
				filterSlots = builder.comment("Number of fluid filter slots").defineInRange("filterSlots", 4, 1, 20);
				maxInputOutput = builder.comment("How much mB can be transfered in / out per operation. This is a base transfer rate that gets multiplied by number of rows in backpack and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
				stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to max input/output value. Value lower than 1 makes stack multiplier affect the capacity less, higher makes it affect the capacity more. 0 turns off stack multiplier affecting input/output").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
				builder.pop();
			}
		}

		public static class EntityBackpackAdditionsConfig {
			private static final String REGISTRY_NAME_MATCHER = "([a-z1-9_.-]+:[a-z1-9_/.-]+)";
			private static final String ENTITY_LOOT_MATCHER = "([a-z1-9_.-]+:[a-z1-9_/.-]+)\\|(null|[a-z1-9_.-]+:[a-z1-9/_.-]+)";
			public final ForgeConfigSpec.DoubleValue chance;
			public final ForgeConfigSpec.BooleanValue addLoot;
			public final ForgeConfigSpec.BooleanValue buffWithPotionEffects;
			public final ForgeConfigSpec.BooleanValue buffHealth;
			public final ForgeConfigSpec.BooleanValue equipWithArmor;
			public final ForgeConfigSpec.BooleanValue playJukebox;
			public final ForgeConfigSpec.DoubleValue backpackDropChance;
			public final ForgeConfigSpec.DoubleValue lootingChanceIncreasePerLevel;
			public final ForgeConfigSpec.ConfigValue<List<? extends String>> entityLootTableList;
			public final ForgeConfigSpec.ConfigValue<List<? extends String>> discBlockList;
			@Nullable
			private Map<EntityType<?>, ResourceLocation> entityLootTables = null;

			public EntityBackpackAdditionsConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Settings for Spawning Entities with Backpack").push("entityBackpackAdditions");
				chance = builder.comment("Chance of an entity spawning with Backpack").defineInRange("chance", 0.01, 0, 1);
				addLoot = builder.comment("Turns on/off addition of loot into backpacks").define("addLoot", true);
				buffWithPotionEffects = builder.comment("Turns on/off buffing the entity that wears backpack with potion effects. These are scaled based on how much loot is added.")
						.define("buffWithPotionEffects", true);
				buffHealth = builder.comment("Turns on/off buffing the entity that wears backpack with additional health. Health is scaled based on backpack tier the mob wears.")
						.define("buffHealth", true);
				equipWithArmor = builder.comment("Turns on/off equiping the entity that wears backpack with armor. What armor material and how enchanted is scaled based on backpack tier the mob wears.")
						.define("equipWithArmor", true);
				entityLootTableList = builder.comment("Map of entities that can spawn with backpack and related loot tables (if adding a loot is enabled) in format of \"EntityRegistryName|LootTableName\"")
						.defineList("entityLootTableList", this::getDefaultEntityLootTableList, mapping -> ((String) mapping).matches(ENTITY_LOOT_MATCHER));
				discBlockList = builder.comment("List of music discs that are not supposed to be played by entities")
						.defineList("discBlockList", this::getDefaultDiscBlockList, mapping -> ((String) mapping).matches(REGISTRY_NAME_MATCHER));
				playJukebox = builder.comment("Turns on/off a chance that the entity that wears backpack gets jukebox upgrade and plays a music disc.").define("playJukebox", true);
				backpackDropChance = builder.comment("Chance of mob dropping backpack when killed by player").defineInRange("backpackDropChance", 0.085, 0, 1);
				lootingChanceIncreasePerLevel = builder.comment("Chance increase per looting level of mob dropping backpack").defineInRange("lootingChanceIncreasePerLevel", 0.01, 0, 0.2);
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

					EntityType<?> entityType = ForgeRegistries.ENTITIES.getValue(new ResourceLocation(entityRegistryName));
					if (entityType != null) {
						entityLootTables.put(entityType, lootTableName.equals("null") ? null : new ResourceLocation(lootTableName));
					}
				}
			}

			private List<String> getDefaultDiscBlockList() {
				List<String> ret = new ArrayList<>();
				ret.add("botania:record_gaia_1");
				ret.add("botania:record_gaia_2");
				return ret;
			}

			private List<String> getDefaultEntityLootTableList() {
				return getDefaultEntityLootMapping().entrySet().stream().map(e -> e.getKey().getRegistryName() + "|" + e.getValue()).collect(Collectors.toList());
			}

			private Map<EntityType<?>, ResourceLocation> getDefaultEntityLootMapping() {
				Map<EntityType<?>, ResourceLocation> mapping = new LinkedHashMap<>();
				mapping.put(EntityType.CREEPER, LootTables.DESERT_PYRAMID);
				mapping.put(EntityType.DROWNED, LootTables.SHIPWRECK_TREASURE);
				mapping.put(EntityType.ENDERMAN, LootTables.END_CITY_TREASURE);
				mapping.put(EntityType.EVOKER, LootTables.WOODLAND_MANSION);
				mapping.put(EntityType.HUSK, LootTables.DESERT_PYRAMID);
				mapping.put(EntityType.PIGLIN, LootTables.BASTION_BRIDGE);
				mapping.put(EntityType.PIGLIN_BRUTE, LootTables.BASTION_TREASURE);
				mapping.put(EntityType.PILLAGER, LootTables.PILLAGER_OUTPOST);
				mapping.put(EntityType.SKELETON, LootTables.SIMPLE_DUNGEON);
				mapping.put(EntityType.STRAY, LootTables.IGLOO_CHEST);
				mapping.put(EntityType.VEX, LootTables.WOODLAND_MANSION);
				mapping.put(EntityType.VINDICATOR, LootTables.WOODLAND_MANSION);
				mapping.put(EntityType.WITCH, LootTables.BURIED_TREASURE);
				mapping.put(EntityType.WITHER_SKELETON, LootTables.NETHER_BRIDGE);
				mapping.put(EntityType.ZOMBIE, LootTables.SIMPLE_DUNGEON);
				mapping.put(EntityType.ZOMBIE_VILLAGER, LootTables.VILLAGE_ARMORER);
				mapping.put(EntityType.ZOMBIFIED_PIGLIN, LootTables.BASTION_OTHER);
				return mapping;
			}
		}

		public static class ToolSwapperUpgradeConfig {
			public final ForgeConfigSpec.IntValue slotsInRow;

			protected ToolSwapperUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Tool Swapper Upgrade" + SETTINGS).push("toolSwapperUpgrade");
				slotsInRow = builder.comment("Number of tool filter slots displayed in a row").defineInRange("slotsInRow", 4, 1, 6);
				builder.pop();
			}
		}

		public static class TankUpgradeConfig {
			public final ForgeConfigSpec.IntValue capacityPerSlotRow;
			public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
			public final ForgeConfigSpec.IntValue autoFillDrainContainerCooldown;
			public final ForgeConfigSpec.IntValue maxInputOutput;

			protected TankUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Tank Upgrade" + SETTINGS).push("tankUpgrade");
				capacityPerSlotRow = builder.comment("Capacity in mB the tank upgrade will have per row of backpack slots").defineInRange("capacityPerSlotRow", 4000, 500, 20000);
				stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to tank capacity. Value lower than 1 makes stack multiplier affect the capacity less, higher makes it affect the capacity more. 0 turns off stack multiplier affecting tank capacity").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
				autoFillDrainContainerCooldown = builder.comment("Cooldown between fill/drain actions done on fluid containers in tank slots. Only fills/drains one bucket worth to/from container after this cooldown and then waits again.").defineInRange("autoFillDrainContainerCooldown", 20, 1, 100);
				maxInputOutput = builder.comment("How much mB can be transfered in / out per operation. This is a base transfer rate and same as max tank capacity gets multiplied by number of rows in backpack and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
				builder.pop();
			}
		}

		public static class BatteryUpgradeConfig {
			public final ForgeConfigSpec.IntValue energyPerSlotRow;
			public final ForgeConfigSpec.DoubleValue stackMultiplierRatio;
			public final ForgeConfigSpec.IntValue maxInputOutput;

			protected BatteryUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Tank Upgrade" + SETTINGS).push("tankUpgrade");
				energyPerSlotRow = builder.comment("Energy in FE the battery upgrade will have per row of backpack slots").defineInRange("energyPerSlotRow", 10000, 500, 50000);
				stackMultiplierRatio = builder.comment("Ratio that gets applied (multiplies) to inventory stack multiplier before this is applied to max energy of the battery and max in/out. Value lower than 1 makes stack multiplier affect the max energy less, higher makes it affect the max energy more. 0 turns off stack multiplier affecting battery upgrade").defineInRange("stackMultiplierRatio", 1D, 0D, 5D);
				maxInputOutput = builder.comment("How much FE can be transfered in / out per operation. This is a base transfer rate and same as max storage gets multiplied by number of rows in backpack and stack multiplier.").defineInRange("maxInputOutput", 20, 1, 1000);
				builder.pop();
			}
		}

		public static class InceptionUpgradeConfig {
			public final ForgeConfigSpec.BooleanValue upgradesUseInventoriesOfBackpacksInBackpack;
			public final ForgeConfigSpec.BooleanValue upgradesInContainedBackpacksAreFunctional;

			public InceptionUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Inception Upgrade Settings").push("inceptionUpgrade");
				upgradesUseInventoriesOfBackpacksInBackpack = builder.comment("Allows / Disallows backpack upgrades to work with inventories of Backpacks in the Backpack with Inception Upgrade")
						.define("upgradesUseInventoriesOfBackpacksInBackpack", true);
				upgradesInContainedBackpacksAreFunctional = builder.comment("Allows / Disallows upgrades to be functional even when they are in Backpacks in the inventory of Backpack with Inception Upgrade")
						.define("upgradesInContainedBackpacksAreFunctional", true);
				builder.pop();
			}
		}

		public static class AutoCookingUpgradeConfig extends CookingUpgradeConfig {
			public final ForgeConfigSpec.IntValue inputFilterSlots;
			public final ForgeConfigSpec.IntValue inputFilterSlotsInRow;
			public final ForgeConfigSpec.IntValue fuelFilterSlots;
			public final ForgeConfigSpec.IntValue fuelFilterSlotsInRow;

			public AutoCookingUpgradeConfig(ForgeConfigSpec.Builder builder, String upgradeName, String path) {
				super(builder, upgradeName, path);
				inputFilterSlots = builder.comment("Number of input filter slots").defineInRange("inputFilterSlots", 8, 1, 20);
				inputFilterSlotsInRow = builder.comment("Number of input filter slots displayed in a row").defineInRange("inputFilterSlotsInRow", 4, 1, 6);
				fuelFilterSlots = builder.comment("Number of fuel filter slots").defineInRange("fuelFilterSlots", 4, 1, 20);
				fuelFilterSlotsInRow = builder.comment("Number of fuel filter slots displayed in a row").defineInRange("fuelFilterSlotsInRow", 4, 1, 6);
				builder.pop();
			}
		}

		public static class CookingUpgradeConfig {
			public final ForgeConfigSpec.DoubleValue cookingSpeedMultiplier;
			public final ForgeConfigSpec.DoubleValue fuelEfficiencyMultiplier;

			protected CookingUpgradeConfig(ForgeConfigSpec.Builder builder, final String upgradeName, String path) {
				builder.comment(upgradeName + SETTINGS).push(path);
				cookingSpeedMultiplier = builder.comment("Smelting speed multiplier (1.0 equals speed at which vanilla furnace smelts items)")
						.defineInRange("smeltingSpeedMultiplier", 1.0D, 0.25D, 4.0D);
				fuelEfficiencyMultiplier = builder.comment("Fuel efficiency multiplier (1.0 equals speed at which it's used in vanilla furnace)")
						.defineInRange("fuelEfficiencyMultiplier", 1.0D, 0.25D, 4.0D);
			}

			public static CookingUpgradeConfig getInstance(ForgeConfigSpec.Builder builder, final String upgradeName, String path) {
				CookingUpgradeConfig instance = new CookingUpgradeConfig(builder, upgradeName, path);
				builder.pop();
				return instance;
			}
		}

		public static class MagnetUpgradeConfig extends FilteredUpgradeConfigBase {
			public final ForgeConfigSpec.IntValue magnetRange;

			public MagnetUpgradeConfig(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow, int defaultMagnetRange) {
				super(builder, name, path, defaultFilterSlots, defaultSlotsInRow);
				magnetRange = builder.comment("Range around backpack in blocks at which magnet will pickup items").defineInRange("magnetRange", defaultMagnetRange, 1, 20);
				builder.pop();
			}
		}

		public static class FilteredUpgradeConfig extends FilteredUpgradeConfigBase {
			public FilteredUpgradeConfig(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow) {
				super(builder, name, path, defaultFilterSlots, defaultSlotsInRow);
				builder.pop();
			}
		}

		public static class FilteredUpgradeConfigBase {
			public final ForgeConfigSpec.IntValue filterSlots;
			public final ForgeConfigSpec.IntValue slotsInRow;

			protected FilteredUpgradeConfigBase(ForgeConfigSpec.Builder builder, String name, String path, int defaultFilterSlots, int defaultSlotsInRow) {
				builder.comment(name + SETTINGS).push(path);
				filterSlots = builder.comment("Number of " + name + "'s filter slots").defineInRange("filterSlots", defaultFilterSlots, 1, 20);
				slotsInRow = builder.comment("Number of filter slots displayed in a row").defineInRange("slotsInRow", defaultSlotsInRow, 1, 6);
			}
		}

		public static class BackpackConfig {
			public final ForgeConfigSpec.IntValue inventorySlotCount;
			public final ForgeConfigSpec.IntValue upgradeSlotCount;

			public BackpackConfig(ForgeConfigSpec.Builder builder, String backpackPrefix, int inventorySlotCountDefault, int upgradeSlotCountDefault) {
				builder.comment(backpackPrefix + " Backpack Settings").push(backpackPrefix.toLowerCase(Locale.ENGLISH) + "Backpack");
				inventorySlotCount = builder.comment("Number of inventory slots in the backpack").defineInRange("inventorySlotCount", inventorySlotCountDefault, 1, 144);
				upgradeSlotCount = builder.comment("Number of upgrade slots in the backpack").defineInRange("upgradeSlotCount", upgradeSlotCountDefault, 0, 10);
				builder.pop();
			}
		}

		public static class EnabledItems {
			private final ForgeConfigSpec.ConfigValue<List<String>> itemsEnableList;
			private final Map<String, Boolean> enabledMap = new ConcurrentHashMap<>();

			EnabledItems(ForgeConfigSpec.Builder builder) {
				itemsEnableList = builder.comment("Disable / enable any items here (disables their recipes)").define("enabledItems", new ArrayList<>());
			}

			public boolean isItemEnabled(Item item) {
				return RegistryHelper.getRegistryName(item).map(rn -> isItemEnabled(rn.getPath())).orElse(false);
			}

			public boolean isItemEnabled(String itemRegistryName) {
				if (!COMMON_SPEC.isLoaded()) {
					return true;
				}
				if (enabledMap.isEmpty()) {
					loadEnabledMap();
				}
				return enabledMap.computeIfAbsent(itemRegistryName, irn -> {
					addEnabledItemToConfig(itemRegistryName);
					return true;
				});
			}

			private void addEnabledItemToConfig(String itemRegistryName) {
				itemsEnableList.get().add(itemRegistryName + ":true");
				COMMON_SPEC.save();
			}

			private void loadEnabledMap() {
				for (String itemEnabled : itemsEnableList.get()) {
					String[] data = itemEnabled.split(":");
					if (data.length == 2) {
						enabledMap.put(data[0], Boolean.valueOf(data[1]));
					} else {
						SophisticatedBackpacks.LOGGER.error("Wrong data for enabledItems - expected name:true/false when {} was provided", itemEnabled);
					}
				}
			}
		}

		public static class DisallowedItems {
			private final ForgeConfigSpec.ConfigValue<List<String>> disallowedItemsList;
			private boolean setInitialized = false;
			private Set<Item> disallowedItemsSet = null;

			DisallowedItems(ForgeConfigSpec.Builder builder) {
				disallowedItemsList = builder.comment("List of items that are not allowed to be put in backpacks - e.g. \"minecraft:shulker_box\"").define("disallowedItems", new ArrayList<>());
			}

			public boolean isItemDisallowed(Item item) {
				if (!COMMON_SPEC.isLoaded()) {
					return true;
				}
				if (!setInitialized) {
					loadDisallowedSet();
				}
				return disallowedItemsSet.contains(item);
			}

			private void loadDisallowedSet() {
				disallowedItemsSet = new HashSet<>();

				for (String disallowedItemName : disallowedItemsList.get()) {
					ResourceLocation registryName = new ResourceLocation(disallowedItemName);
					if (ForgeRegistries.ITEMS.containsKey(registryName)) {
						disallowedItemsSet.add(ForgeRegistries.ITEMS.getValue(registryName));
					}
				}
			}
		}

		public static class StackUpgradeConfig {
			private final ForgeConfigSpec.ConfigValue<List<String>> nonStackableItemsList;
			@Nullable
			private Set<Item> nonStackableItems = null;
			public StackUpgradeConfig(ForgeConfigSpec.Builder builder) {
				builder.comment("Stack Upgrade" + SETTINGS).push("stackUpgrade");
				nonStackableItemsList =  builder.comment("List of items that are not supposed to stack in backpack even when stack upgrade is inserted. Item registry names are expected here.").define("nonStackableItems", new ArrayList<>());
				builder.pop();
			}

			public boolean canItemStack(Item item) {
				if (!COMMON_SPEC.isLoaded()) {
					return true;
				}
				if (nonStackableItems == null) {
					nonStackableItems = new HashSet<>();
					nonStackableItemsList.get().forEach(name -> {
						ResourceLocation registryName = new ResourceLocation(name);
						if (ForgeRegistries.ITEMS.containsKey(registryName)) {
							nonStackableItems.add(ForgeRegistries.ITEMS.getValue(registryName));
						} else {
							SophisticatedBackpacks.LOGGER.error("Item {} is set to be disabled in config, but it does not exist in item registry", name);
						}
					});
				}
				return !nonStackableItems.contains(item);
			}
		}
	}
}
