package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraft.entity.EntityType;
import net.minecraft.loot.LootTables;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SortButtonsPosition;
import org.apache.commons.lang3.tuple.Pair;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

public class Config {
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
		public final SmeltingUpgradeConfig smeltingUpgrade;
		public final AutoSmeltingUpgradeConfig autoSmeltingUpgrade;
		public final InceptionUpgradeConfig inceptionUpgrade;
		public final EntityBackpackAdditionsConfig entityBackpackAdditions;
		public final ForgeConfigSpec.BooleanValue chestLootEnabled;
		public final ForgeConfigSpec.BooleanValue shiftClickIntoOpenTabFirst;

		Common(ForgeConfigSpec.Builder builder) {
			builder.comment("Common Settings").push("common");

			enabledItems = new EnabledItems(builder);

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
			smeltingUpgrade = new SmeltingUpgradeConfig(builder);
			autoSmeltingUpgrade = new AutoSmeltingUpgradeConfig(builder);
			inceptionUpgrade = new InceptionUpgradeConfig(builder);
			entityBackpackAdditions = new EntityBackpackAdditionsConfig(builder);

			chestLootEnabled = builder.comment("Turns on/off loot added to various vanilla chest loot tables").define("chestLootEnabled", true);

			shiftClickIntoOpenTabFirst = builder.comment("Shift clicking will first move the stack into open tab and only then to player's inventory or to backpack (based on where shift clicking from backpack or from player's inventory).",
					"Setting this to false will move stacks to backpack/inventory first.").define("shiftClickIntoOpenTabFirst", true);

			builder.pop();
		}

		public static class EntityBackpackAdditionsConfig {
			private static final String REGISTRY_NAME_MATCHER = "([a-z1-9_.-]+:[a-z1-9_/.-]+)\\|(null|[a-z1-9_.-]+:[a-z1-9/_.-]+)";
			public final ForgeConfigSpec.DoubleValue chance;
			public final ForgeConfigSpec.BooleanValue addLoot;
			public final ForgeConfigSpec.BooleanValue buffWithPotionEffects;
			public final ForgeConfigSpec.BooleanValue buffHealth;
			public final ForgeConfigSpec.BooleanValue equipWithArmor;
			public final ForgeConfigSpec.BooleanValue playJukebox;
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
						.defineList("entityLootTableList", this::getDefaultEntityLootTableList, mapping -> ((String) mapping).matches(REGISTRY_NAME_MATCHER));
				discBlockList = builder.comment("List of music discs that are not supposed to be played by entities")
						.defineList("discBlockList", this::getDefaultDiscBlockList, mapping -> ((String) mapping).matches(REGISTRY_NAME_MATCHER));
				playJukebox = builder.comment("Turns on/off a chance that the entity that wears backpack gets jukebox upgrade a plays a music disc.").define("playJukebox", true);
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
				mapping.put(EntityType.CREEPER, LootTables.CHESTS_DESERT_PYRAMID);
				mapping.put(EntityType.DROWNED, LootTables.CHESTS_SHIPWRECK_TREASURE);
				mapping.put(EntityType.ENDERMAN, LootTables.CHESTS_END_CITY_TREASURE);
				mapping.put(EntityType.EVOKER, LootTables.CHESTS_WOODLAND_MANSION);
				mapping.put(EntityType.HUSK, LootTables.CHESTS_DESERT_PYRAMID);
				mapping.put(EntityType.PIGLIN, LootTables.BASTION_BRIDGE);
				mapping.put(EntityType.field_242287_aj, LootTables.BASTION_TREASURE);
				mapping.put(EntityType.PILLAGER, LootTables.CHESTS_PILLAGER_OUTPOST);
				mapping.put(EntityType.SKELETON, LootTables.CHESTS_SIMPLE_DUNGEON);
				mapping.put(EntityType.STRAY, LootTables.CHESTS_IGLOO_CHEST);
				mapping.put(EntityType.VEX, LootTables.CHESTS_WOODLAND_MANSION);
				mapping.put(EntityType.VINDICATOR, LootTables.CHESTS_WOODLAND_MANSION);
				mapping.put(EntityType.WITCH, LootTables.CHESTS_BURIED_TREASURE);
				mapping.put(EntityType.WITHER_SKELETON, LootTables.CHESTS_NETHER_BRIDGE);
				mapping.put(EntityType.ZOMBIE, LootTables.CHESTS_SIMPLE_DUNGEON);
				mapping.put(EntityType.ZOMBIE_VILLAGER, LootTables.CHESTS_VILLAGE_VILLAGE_ARMORER);
				mapping.put(EntityType.ZOMBIFIED_PIGLIN, LootTables.BASTION_OTHER);
				return mapping;
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

		public static class AutoSmeltingUpgradeConfig extends SmeltingUpgradeConfigBase {
			public final ForgeConfigSpec.IntValue inputFilterSlots;
			public final ForgeConfigSpec.IntValue inputFilterSlotsInRow;
			public final ForgeConfigSpec.IntValue fuelFilterSlots;
			public final ForgeConfigSpec.IntValue fuelFilterSlotsInRow;

			public AutoSmeltingUpgradeConfig(ForgeConfigSpec.Builder builder) {
				super(builder, "Auto-Smelting Upgrade", "autoSmeltingUpgrade");
				inputFilterSlots = builder.comment("Number of input filter slots").defineInRange("inputFilterSlots", 8, 1, 20);
				inputFilterSlotsInRow = builder.comment("Number of input filter slots displayed in a row").defineInRange("inputFilterSlotsInRow", 4, 1, 6);
				fuelFilterSlots = builder.comment("Number of fuel filter slots").defineInRange("fuelFilterSlots", 4, 1, 20);
				fuelFilterSlotsInRow = builder.comment("Number of fuel filter slots displayed in a row").defineInRange("fuelFilterSlotsInRow", 4, 1, 6);
				builder.pop();
			}
		}

		public static class SmeltingUpgradeConfig extends SmeltingUpgradeConfigBase {
			public SmeltingUpgradeConfig(ForgeConfigSpec.Builder builder) {
				super(builder, "Smelting Upgrade", "smeltingUpgrade");
				builder.pop();
			}
		}

		public static class SmeltingUpgradeConfigBase {
			public final ForgeConfigSpec.DoubleValue smeltingSpeedMultiplier;
			public final ForgeConfigSpec.DoubleValue fuelEfficiencyMultiplier;

			protected SmeltingUpgradeConfigBase(ForgeConfigSpec.Builder builder, final String upgradeName, String path) {
				builder.comment(upgradeName + " Settings").push(path);
				smeltingSpeedMultiplier = builder.comment("Smelting speed multiplier (1.0 equals speed at which vanilla furnace smelts items)")
						.defineInRange("smeltingSpeedMultiplier", 1.0D, 0.25D, 4.0D);
				fuelEfficiencyMultiplier = builder.comment("Fuel efficiency multiplier (1.0 equals speed at which it's used in vanilla furnace)")
						.defineInRange("fuelEfficiencyMultiplier", 1.0D, 0.25D, 4.0D);
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
				builder.comment(name + " Settings").push(path);
				filterSlots = builder.comment("Number of " + name + "'s filter slots").defineInRange("filterSlots", defaultFilterSlots, 1, 20);
				slotsInRow = builder.comment("Number of filter slots displayed in a row").defineInRange("slotsInRow", defaultSlotsInRow, 1, 6);
			}
		}

		public static class BackpackConfig {
			public final ForgeConfigSpec.IntValue inventorySlotCount;
			public final ForgeConfigSpec.IntValue upgradeSlotCount;

			public BackpackConfig(ForgeConfigSpec.Builder builder, String backpackPrefix, int inventorySlotCountDefault, int upgradeSlotCountDefault) {
				builder.comment(backpackPrefix + " Backpack Settings").push(backpackPrefix.toLowerCase() + "Backpack");
				inventorySlotCount = builder.comment("Number of inventory slots in the backpack").defineInRange("inventorySlotCount", inventorySlotCountDefault, 1, 144);
				upgradeSlotCount = builder.comment("Number of upgrade slots in the backpack").defineInRange("upgradeSlotCount", upgradeSlotCountDefault, 0, 10);
				builder.pop();
			}
		}

		public static class EnabledItems {
			private final ForgeConfigSpec.ConfigValue<List<String>> itemsEnableList;
			private final Map<String, Boolean> enabledMap = new HashMap<>();

			EnabledItems(ForgeConfigSpec.Builder builder) {
				itemsEnableList = builder.comment("Disable / enable any items here (disables their recipes)").define("enabledItems", new ArrayList<>());
			}

			public boolean isItemEnabled(String itemRegistryName) {
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
	}
}
