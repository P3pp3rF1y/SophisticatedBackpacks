package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.SharedConstants;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.ByteTag;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.FloatTag;
import net.minecraft.nbt.IntTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.item.component.ItemContainerContents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InventoryOrder;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapMode;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterAttributes;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.PrimaryMatch;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.HungerLevel;
import net.p3pp3rf1y.sophisticatedcore.upgrades.filter.Direction;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidType;
import net.p3pp3rf1y.sophisticatedcore.util.LegacyUpgradeDataMigration;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestFactory;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class LegacyBackpackUpgradeDataMigrationTest {
	private static final RegistryAccess REGISTRIES = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
	private static final FilterAttributes ALLOW_LIST_FILTER_ATTRIBUTES = new FilterAttributes(Set.of(), true, false, false, PrimaryMatch.ITEM, true,
			ItemContainerContents.EMPTY, false, false);
	private static final List<MigrationCase> MIGRATION_CASES = List.of(rootFilterCase("pickup_upgrade", ModItems.PICKUP_UPGRADE.get()),
			rootFilterCase("advanced_pickup_upgrade", ModItems.ADVANCED_PICKUP_UPGRADE.get()),
			state("filter_upgrade", ModItems.FILTER_UPGRADE.get(), "direction", StringTag.valueOf("input"), ModCoreDataComponents.DIRECTION.get(),
					Direction.INPUT),
			state("advanced_filter_upgrade", ModItems.ADVANCED_FILTER_UPGRADE.get(), "direction", StringTag.valueOf("input"),
					ModCoreDataComponents.DIRECTION.get(), Direction.INPUT),
			state("magnet_upgrade", ModItems.MAGNET_UPGRADE.get(), "pickupItems", ByteTag.valueOf(false), ModCoreDataComponents.PICKUP_ITEMS.get(), false),
			state("advanced_magnet_upgrade", ModItems.ADVANCED_MAGNET_UPGRADE.get(), "pickupItems", ByteTag.valueOf(false),
					ModCoreDataComponents.PICKUP_ITEMS.get(), false),
			state("feeding_upgrade", ModItems.FEEDING_UPGRADE.get(), "feedAtHungerLevel", StringTag.valueOf("full"),
					ModCoreDataComponents.FEED_AT_HUNGER_LEVEL.get(), HungerLevel.FULL),
			state("advanced_feeding_upgrade", ModItems.ADVANCED_FEEDING_UPGRADE.get(), "feedAtHungerLevel", StringTag.valueOf("full"),
					ModCoreDataComponents.FEED_AT_HUNGER_LEVEL.get(), HungerLevel.FULL),
			state("compacting_upgrade", ModItems.COMPACTING_UPGRADE.get(), "compactNonUncraftable", ByteTag.valueOf(true),
					ModCoreDataComponents.COMPACT_NON_UNCRAFTABLE.get(), true),
			state("advanced_compacting_upgrade", ModItems.ADVANCED_COMPACTING_UPGRADE.get(), "compactNonUncraftable", ByteTag.valueOf(true),
					ModCoreDataComponents.COMPACT_NON_UNCRAFTABLE.get(), true),
			state("void_upgrade", ModItems.VOID_UPGRADE.get(), "voidType", StringTag.valueOf("storage_overflow"), ModCoreDataComponents.VOID_TYPE.get(),
					VoidType.STORAGE_OVERFLOW),
			state("advanced_void_upgrade", ModItems.ADVANCED_VOID_UPGRADE.get(), "voidType", StringTag.valueOf("storage_overflow"),
					ModCoreDataComponents.VOID_TYPE.get(), VoidType.STORAGE_OVERFLOW),
			rootFilterCase("restock_upgrade", ModItems.RESTOCK_UPGRADE.get()),
			rootFilterCase("advanced_restock_upgrade", ModItems.ADVANCED_RESTOCK_UPGRADE.get()),
			rootFilterCase("deposit_upgrade", ModItems.DEPOSIT_UPGRADE.get()),
			rootFilterCase("advanced_deposit_upgrade", ModItems.ADVANCED_DEPOSIT_UPGRADE.get()), refillCase("refill_upgrade", ModItems.REFILL_UPGRADE.get()),
			refillCase("advanced_refill_upgrade", ModItems.ADVANCED_REFILL_UPGRADE.get()),
			state("inception_upgrade", ModItems.INCEPTION_UPGRADE.get(), "inventoryOrder", StringTag.valueOf("main_first"),
					ModDataComponents.INVENTORY_ORDER.get(), InventoryOrder.MAIN_FIRST),
			state("smelting_upgrade", ModItems.SMELTING_UPGRADE.get(), "storedXp", FloatTag.valueOf(2.5F), ModCoreDataComponents.STORED_XP.get(), 2.5F),
			autoCookingCase("auto_smelting_upgrade", ModItems.AUTO_SMELTING_UPGRADE.get()),
			state("smoking_upgrade", ModItems.SMOKING_UPGRADE.get(), "storedXp", FloatTag.valueOf(2.5F), ModCoreDataComponents.STORED_XP.get(), 2.5F),
			autoCookingCase("auto_smoking_upgrade", ModItems.AUTO_SMOKING_UPGRADE.get()),
			state("blasting_upgrade", ModItems.BLASTING_UPGRADE.get(), "storedXp", FloatTag.valueOf(2.5F), ModCoreDataComponents.STORED_XP.get(), 2.5F),
			autoCookingCase("auto_blasting_upgrade", ModItems.AUTO_BLASTING_UPGRADE.get()),
			state("crafting_upgrade", ModItems.CRAFTING_UPGRADE.get(), "shiftClickIntoStorage", ByteTag.valueOf(false),
					ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE.get(), false),
			state("stonecutter_upgrade", ModItems.STONECUTTER_UPGRADE.get(), "recipeId", StringTag.valueOf("minecraft:stonecutting"),
					ModCoreDataComponents.RECIPE_ID.get(), ResourceLocation.withDefaultNamespace("stonecutting")),
			commonEnabledCase("stack_upgrade_starter_tier", ModItems.STACK_UPGRADE_STARTER_TIER.get()),
			commonEnabledCase("stack_upgrade_tier_1", ModItems.STACK_UPGRADE_TIER_1.get()),
			commonEnabledCase("stack_upgrade_tier_2", ModItems.STACK_UPGRADE_TIER_2.get()),
			commonEnabledCase("stack_upgrade_tier_3", ModItems.STACK_UPGRADE_TIER_3.get()),
			commonEnabledCase("stack_upgrade_tier_4", ModItems.STACK_UPGRADE_TIER_4.get()),
			commonEnabledCase("stack_downgrade_tier_1", ModItems.STACK_DOWNGRADE_TIER_1.get()),
			commonEnabledCase("stack_downgrade_tier_2", ModItems.STACK_DOWNGRADE_TIER_2.get()),
			commonEnabledCase("stack_downgrade_tier_3", ModItems.STACK_DOWNGRADE_TIER_3.get()),
			commonEnabledCase("stack_upgrade_omega_tier", ModItems.STACK_UPGRADE_OMEGA_TIER.get()),
			state("jukebox_upgrade", ModItems.JUKEBOX_UPGRADE.get(), "isPlaying", ByteTag.valueOf(true), ModCoreDataComponents.IS_PLAYING.get(), true),
			state("advanced_jukebox_upgrade", ModItems.ADVANCED_JUKEBOX_UPGRADE.get(), "isPlaying", ByteTag.valueOf(true),
					ModCoreDataComponents.IS_PLAYING.get(), true),
			state("tool_swapper_upgrade", ModItems.TOOL_SWAPPER_UPGRADE.get(), "shouldSwapWeapon", ByteTag.valueOf(false),
					ModDataComponents.SHOULD_SWAP_WEAPON.get(), false),
			state("advanced_tool_swapper_upgrade", ModItems.ADVANCED_TOOL_SWAPPER_UPGRADE.get(), "shouldSwapWeapon", ByteTag.valueOf(false),
					ModDataComponents.SHOULD_SWAP_WEAPON.get(), false),
			state("tank_upgrade", ModItems.TANK_UPGRADE.get(), "inventory", emptyInventoryData(), ModCoreDataComponents.LENIENT_CONTAINER.get(),
					ItemContainerContents.EMPTY),
			state("battery_upgrade", ModItems.BATTERY_UPGRADE.get(), "energyStored", IntTag.valueOf(1), ModCoreDataComponents.ENERGY_STORED.get(), 1),
			state("pump_upgrade", ModItems.PUMP_UPGRADE.get(), "input", ByteTag.valueOf(false), ModCoreDataComponents.IS_INPUT.get(), false),
			state("advanced_pump_upgrade", ModItems.ADVANCED_PUMP_UPGRADE.get(), "input", ByteTag.valueOf(false), ModCoreDataComponents.IS_INPUT.get(), false),
			state("xp_pump_upgrade", ModItems.XP_PUMP_UPGRADE.get(), "level", IntTag.valueOf(1), ModCoreDataComponents.LEVEL.get(), 1),
			state("anvil_upgrade", ModItems.ANVIL_UPGRADE.get(), "itemName", StringTag.valueOf("Migrated"), ModDataComponents.ITEM_NAME.get(), "Migrated"),
			state("smithing_upgrade", ModItems.SMITHING_UPGRADE.get(), "shiftClickIntoStorage", ByteTag.valueOf(false),
					ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE.get(), false),
			commonEnabledCase("everlasting_upgrade", ModItems.EVERLASTING_UPGRADE.get()),
			commonEnabledCase("infinity_upgrade", ModItems.INFINITY_UPGRADE.get()),
			commonEnabledCase("survival_infinity_upgrade", ModItems.SURVIVAL_INFINITY_UPGRADE.get()),
			state("alchemy_upgrade", ModItems.ALCHEMY_UPGRADE.get(), "matchAllEffects", ByteTag.valueOf(false), ModCoreDataComponents.MATCH_ALL_EFFECTS.get(),
					false),
			state("advanced_alchemy_upgrade", ModItems.ADVANCED_ALCHEMY_UPGRADE.get(), "matchAllEffects", ByteTag.valueOf(false),
					ModCoreDataComponents.MATCH_ALL_EFFECTS.get(), false),
			commonEnabledCase("mob_catcher_upgrade", ModItems.MOB_CATCHER_UPGRADE.get()),
			commonEnabledCase("advanced_mob_catcher_upgrade", ModItems.ADVANCED_MOB_CATCHER_UPGRADE.get()));

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		LegacyBackpackUpgradeDataMigration.registerMigrators();
	}

	@Test
	void migratesToolSwapperAndRefillFieldsWithLegacyFilterDefaults() {
		ItemStack toolSwapper = new ItemStack(Items.STICK);
		CompoundTag toolSwapperData = rootFilterData();
		toolSwapperData.putBoolean("shouldSwapWeapon", false);
		toolSwapperData.putString("toolSwapMode", "onlyTools");

		assertTrue(LegacyBackpackUpgradeDataMigration.migrateToolSwapper(toolSwapper, toolSwapperData, REGISTRIES));

		assertFalse(toolSwapper.get(ModDataComponents.SHOULD_SWAP_WEAPON));
		assertEquals(ToolSwapMode.ONLY_TOOLS, toolSwapper.get(ModDataComponents.TOOL_SWAP_MODE));
		assertTrue(toolSwapper.get(ModCoreDataComponents.FILTER_ATTRIBUTES).matchAnyTag());
		assertTrue(toolSwapperData.isEmpty());

		ItemStack refill = new ItemStack(Items.STICK);
		CompoundTag refillData = rootFilterData();
		CompoundTag targetSlots = new CompoundTag();
		targetSlots.putString("3", "toolbar_3");
		refillData.put("targetSlots", targetSlots);

		assertTrue(LegacyBackpackUpgradeDataMigration.migrateRefill(refill, refillData, REGISTRIES));

		assertEquals(Map.of(3, RefillUpgradeWrapper.TargetSlot.TOOLBAR_3), refill.get(ModDataComponents.TARGET_SLOTS));
		assertTrue(refillData.isEmpty());

		ItemStack refillWithPartialFilter = new ItemStack(Items.STICK);
		CompoundTag partialFilterData = new CompoundTag();
		partialFilterData.putBoolean("matchNbt", true);
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateRefill(refillWithPartialFilter, partialFilterData, REGISTRIES));
		assertTrue(refillWithPartialFilter.get(ModCoreDataComponents.FILTER_ATTRIBUTES).isAllowList());
	}

	@Test
	void migratesInceptionAndCraftingUpgradeFieldsWithNestedItems() {
		ItemStack inception = new ItemStack(Items.STICK);
		CompoundTag inceptionData = new CompoundTag();
		inceptionData.putString("inventoryOrder", "incepted_first");
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateInception(inception, inceptionData));
		assertEquals(InventoryOrder.INCEPTED_FIRST, inception.get(ModDataComponents.INVENTORY_ORDER));

		ItemStack anvil = new ItemStack(Items.STICK);
		CompoundTag anvilData = craftingInventory(2, 1);
		anvilData.putBoolean("shiftClickIntoStorage", false);
		anvilData.putString("itemName", "Renamed");
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateAnvil(anvil, anvilData, REGISTRIES));
		assertEquals(Items.STONE, anvil.get(ModCoreDataComponents.LENIENT_CONTAINER).getStackInSlot(1).getItem());
		assertFalse(anvil.get(ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE));
		assertEquals("Renamed", anvil.get(ModDataComponents.ITEM_NAME));

		ItemStack smithing = new ItemStack(Items.STICK);
		CompoundTag smithingData = craftingInventory(3, 2);
		smithingData.putBoolean("shiftClickIntoStorage", false);
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateSmithing(smithing, smithingData, REGISTRIES));
		assertEquals(Items.STONE, smithing.get(ModCoreDataComponents.LENIENT_CONTAINER).getStackInSlot(2).getItem());
		assertFalse(smithing.get(ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE));
	}

	@Test
	void migratesDepositAndRestockFiltersAndDropsLegacyDataForExistingComponents() {
		ItemStack deposit = new ItemStack(Items.STICK);
		CompoundTag depositData = rootFilterData();
		depositData.putBoolean("filterByInventory", true);
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateDeposit(deposit, depositData, REGISTRIES));
		assertTrue(deposit.get(ModDataComponents.FILTER_BY_INVENTORY));
		assertTrue(deposit.get(ModCoreDataComponents.FILTER_ATTRIBUTES).filterByInventory());
		assertTrue(depositData.isEmpty());

		ItemStack restock = new ItemStack(Items.STICK);
		FilterAttributes currentAttributes = new FilterAttributes(Set.of(), false, false, false, PrimaryMatch.ITEM, false, ItemContainerContents.EMPTY, false,
				false);
		restock.set(ModCoreDataComponents.FILTER_ATTRIBUTES, currentAttributes);
		CompoundTag restockData = rootFilterData();
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateRestock(restock, restockData, REGISTRIES));
		assertEquals(currentAttributes, restock.get(ModCoreDataComponents.FILTER_ATTRIBUTES));
		assertTrue(restockData.isEmpty());

		ItemStack existingModuleComponent = new ItemStack(Items.STICK);
		existingModuleComponent.set(ModDataComponents.FILTER_BY_INVENTORY, true);
		CompoundTag existingModuleData = rootFilterData();
		existingModuleData.putBoolean("filterByInventory", false);
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateDeposit(existingModuleComponent, existingModuleData, REGISTRIES));
		assertTrue(existingModuleComponent.get(ModDataComponents.FILTER_BY_INVENTORY));
		assertFalse(existingModuleComponent.get(ModCoreDataComponents.FILTER_ATTRIBUTES).filterByInventory());
		assertTrue(existingModuleData.isEmpty());

		ItemStack existingFilterAttributes = new ItemStack(Items.STICK);
		existingFilterAttributes.set(ModCoreDataComponents.FILTER_ATTRIBUTES, currentAttributes);
		CompoundTag malformedModuleData = new CompoundTag();
		malformedModuleData.putString("filterByInventory", "malformed");
		assertFalse(LegacyBackpackUpgradeDataMigration.migrateDeposit(existingFilterAttributes, malformedModuleData, REGISTRIES));
		assertFalse(existingFilterAttributes.has(ModDataComponents.FILTER_BY_INVENTORY));
		assertTrue(malformedModuleData.contains("filterByInventory"));
	}

	@Test
	void retainsMalformedAndUnknownDataAndDoesNotMigrateTwice() {
		ItemStack refill = new ItemStack(Items.STICK);
		CompoundTag data = rootFilterData();
		CompoundTag malformedSlots = new CompoundTag();
		malformedSlots.putString("not-a-slot", "toolbar_3");
		data.put("targetSlots", malformedSlots);
		data.putString("unknown", "retained");

		assertTrue(LegacyBackpackUpgradeDataMigration.migrateRefill(refill, data, REGISTRIES));
		assertTrue(data.contains("targetSlots"));
		assertEquals("retained", data.getString("unknown"));
		assertFalse(LegacyBackpackUpgradeDataMigration.migrateRefill(refill, data, REGISTRIES));

		ItemStack current = new ItemStack(Items.STICK);
		current.set(ModDataComponents.SHOULD_SWAP_WEAPON, true);
		CompoundTag currentData = new CompoundTag();
		currentData.putBoolean("shouldSwapWeapon", false);
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateToolSwapper(current, currentData, REGISTRIES));
		assertTrue(current.get(ModDataComponents.SHOULD_SWAP_WEAPON));
		assertTrue(currentData.isEmpty());

		CompoundTag unknownTargetSlots = new CompoundTag();
		CompoundTag unknownTargetSlot = new CompoundTag();
		unknownTargetSlot.putString("0", "unknown_slot");
		unknownTargetSlots.put("targetSlots", unknownTargetSlot);
		assertFalse(LegacyBackpackUpgradeDataMigration.migrateRefill(new ItemStack(Items.STICK), unknownTargetSlots, REGISTRIES));
		assertTrue(unknownTargetSlots.contains("targetSlots"));

		ItemStack malformedDeposit = new ItemStack(Items.STICK);
		CompoundTag malformedDepositData = new CompoundTag();
		malformedDepositData.putBoolean("filterByInventory", true);
		malformedDepositData.putString("matchNbt", "true");
		assertTrue(LegacyBackpackUpgradeDataMigration.migrateDeposit(malformedDeposit, malformedDepositData, REGISTRIES));
		assertFalse(LegacyBackpackUpgradeDataMigration.migrateDeposit(malformedDeposit, malformedDepositData, REGISTRIES));
		assertTrue(malformedDeposit.get(ModDataComponents.FILTER_BY_INVENTORY));
		assertTrue(malformedDepositData.contains("filterByInventory"));
	}

	@Test
	void dispatchesEveryRegisteredBackpackMigration() {
		assertDispatched(ModItems.TOOL_SWAPPER_UPGRADE.get(), rootFilterData());
		assertDispatched(ModItems.REFILL_UPGRADE.get(), rootFilterData());
		CompoundTag inceptionData = new CompoundTag();
		inceptionData.putString("inventoryOrder", "main_first");
		assertDispatched(ModItems.INCEPTION_UPGRADE.get(), inceptionData);
		assertDispatched(ModItems.ANVIL_UPGRADE.get(), craftingInventory(2, 0));
		assertDispatched(ModItems.SMITHING_UPGRADE.get(), craftingInventory(3, 0));
		assertDispatched(ModItems.DEPOSIT_UPGRADE.get(), rootFilterData());
		assertDispatched(ModItems.RESTOCK_UPGRADE.get(), rootFilterData());
	}

	@TestFactory
	Stream<DynamicTest> migratesEveryConcreteBackpackUpgradeThroughTheFullPipeline() {
		return MIGRATION_CASES.stream().map(migrationCase -> DynamicTest.dynamicTest(migrationCase.name(), () -> assertMigrates(migrationCase)));
	}

	@Test
	void migrationCasesCoverEveryConcreteRegisteredUpgradeItem() {
		Set<Item> caseItems = MIGRATION_CASES.stream().map(MigrationCase::item).collect(Collectors.toSet());
		Set<Item> registeredUpgradeItems = ModItems.ITEMS.getEntries().stream().map(holder -> holder.get()).filter(IUpgradeItem.class::isInstance)
				.collect(Collectors.toSet());

		assertEquals(MIGRATION_CASES.size(), caseItems.size(), "Migration cases must not contain duplicate items");
		assertEquals(54, caseItems.size(), "Expected concrete registered Backpack IUpgradeItem count");
		assertEquals(40, MIGRATION_CASES.stream().filter(migrationCase -> !migrationCase.commonEnabledOnly()).count(),
				"Expected stateful migration case count");
		assertEquals(14, MIGRATION_CASES.stream().filter(MigrationCase::commonEnabledOnly).count(), "Expected common-enabled-only migration case count");
		assertEquals(54, registeredUpgradeItems.size(), "Registered concrete Backpack IUpgradeItem count changed");
		assertEquals(registeredUpgradeItems, caseItems, "Every registered Backpack IUpgradeItem must have a migration case");
	}

	private static void assertMigrates(MigrationCase migrationCase) {
		ItemStack stack = new ItemStack(migrationCase.item());
		CompoundTag legacyData = migrationCase.legacyData().copy();
		legacyData.putString("unknown", "retained");
		stack.set(DataComponents.CUSTOM_DATA, CustomData.of(legacyData));

		assertTrue(LegacyUpgradeDataMigration.migrate(stack, REGISTRIES), migrationCase.name());
		assertComponentEquals(stack, migrationCase.expectedComponent(), migrationCase.expectedValue());
		CompoundTag remainingData = stack.get(DataComponents.CUSTOM_DATA).copyTag();
		assertFalse(remainingData.contains(migrationCase.legacyKey()), migrationCase.name());
		assertEquals("retained", remainingData.getString("unknown"), migrationCase.name());
		assertFalse(LegacyUpgradeDataMigration.migrate(stack, REGISTRIES), migrationCase.name());
	}

	private static MigrationCase rootFilterCase(String name, Item item) {
		return state(name, item, "isAllowList", ByteTag.valueOf(true), ModCoreDataComponents.FILTER_ATTRIBUTES.get(), ALLOW_LIST_FILTER_ATTRIBUTES);
	}

	private static MigrationCase refillCase(String name, Item item) {
		CompoundTag targetSlots = new CompoundTag();
		targetSlots.putString("0", "toolbar_3");
		return state(name, item, "targetSlots", targetSlots, ModDataComponents.TARGET_SLOTS.get(), Map.of(0, RefillUpgradeWrapper.TargetSlot.TOOLBAR_3));
	}

	private static MigrationCase autoCookingCase(String name, Item item) {
		CompoundTag inputFilter = new CompoundTag();
		inputFilter.put("filters", emptyInventoryData());
		return state(name, item, "inputFilter", inputFilter, ModCoreDataComponents.INPUT_FILTER_ATTRIBUTES.get(), ALLOW_LIST_FILTER_ATTRIBUTES);
	}

	private static MigrationCase commonEnabledCase(String name, Item item) {
		CompoundTag legacyData = new CompoundTag();
		legacyData.putBoolean("enabled", false);
		return new MigrationCase(name, item, "enabled", legacyData, ModCoreDataComponents.ENABLED.get(), false, true);
	}

	private static MigrationCase state(String name, Item item, String legacyKey, Tag legacyValue, DataComponentType<?> expectedComponent,
			Object expectedValue) {
		CompoundTag legacyData = new CompoundTag();
		legacyData.put(legacyKey, legacyValue);
		return new MigrationCase(name, item, legacyKey, legacyData, expectedComponent, expectedValue, false);
	}

	private static CompoundTag emptyInventoryData() {
		CompoundTag inventory = new CompoundTag();
		inventory.putInt("Size", 0);
		inventory.put("Items", new ListTag());
		return inventory;
	}

	private static void assertComponentEquals(ItemStack stack, DataComponentType<?> component, Object expectedValue) {
		assertEquals(expectedValue, stack.get(component));
	}

	private record MigrationCase(String name, Item item, String legacyKey, CompoundTag legacyData, DataComponentType<?> expectedComponent, Object expectedValue,
			boolean commonEnabledOnly) {
	}

	private static void assertDispatched(Item upgradeItem, CompoundTag legacyData) {
		ItemStack stack = new ItemStack(upgradeItem);
		stack.set(DataComponents.CUSTOM_DATA, CustomData.of(legacyData));

		assertTrue(LegacyUpgradeDataMigration.migrate(stack, REGISTRIES));
		assertFalse(stack.has(DataComponents.CUSTOM_DATA));
	}

	private static CompoundTag rootFilterData() {
		CompoundTag data = new CompoundTag();
		data.putBoolean("isAllowList", true);
		return data;
	}

	private static CompoundTag craftingInventory(int size, int slot) {
		CompoundTag data = new CompoundTag();
		CompoundTag inventory = new CompoundTag();
		inventory.putInt("Size", size);
		ListTag items = new ListTag();
		CompoundTag item = new CompoundTag();
		item.putString("id", "minecraft:stone");
		item.putByte("Count", (byte) 1);
		item.putInt("Slot", slot);
		items.add(item);
		inventory.put("Items", items);
		data.put("craftingInventory", inventory);
		return data;
	}
}
