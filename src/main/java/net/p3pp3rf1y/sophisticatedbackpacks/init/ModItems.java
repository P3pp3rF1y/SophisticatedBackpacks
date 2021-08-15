package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.block.DispenserBlock;
import net.minecraft.client.gui.ScreenManager;
import net.minecraft.dispenser.IBlockSource;
import net.minecraft.dispenser.OptionalDispenseBehavior;
import net.minecraft.entity.EntityClassification;
import net.minecraft.entity.EntityType;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.DirectionalPlaceContext;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.util.Direction;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.crafting.CraftingHelper;
import net.minecraftforge.common.extensions.IForgeContainerType;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.RegistryObject;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeGuiManager;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SettingsContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.ItemEnabledCondition;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.ContentsFilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryInventoryPart;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery.BatteryUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.AutoSmeltingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.AutoSmeltingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.AutoSmeltingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.AutoSmeltingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter.StonecutterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter.StonecutterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter.StonecutterUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter.StonecutterUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankInventoryPart;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank.TankUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemBase;

public class ModItems {
	private ModItems() {}

	private static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister<ContainerType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister<EntityType<?>> ENTITIES = DeferredRegister.create(ForgeRegistries.ENTITIES, SophisticatedBackpacks.MOD_ID);

	public static final RegistryObject<BackpackItem> BACKPACK = ITEMS.register("backpack",
			() -> new BackpackItem(Config.COMMON.leatherBackpack.inventorySlotCount::get, Config.COMMON.leatherBackpack.upgradeSlotCount::get, ModBlocks.BACKPACK));
	public static final RegistryObject<BackpackItem> IRON_BACKPACK = ITEMS.register("iron_backpack",
			() -> new BackpackItem(Config.COMMON.ironBackpack.inventorySlotCount::get, Config.COMMON.ironBackpack.upgradeSlotCount::get, ModBlocks.IRON_BACKPACK));
	public static final RegistryObject<BackpackItem> GOLD_BACKPACK = ITEMS.register("gold_backpack",
			() -> new BackpackItem(Config.COMMON.goldBackpack.inventorySlotCount::get, Config.COMMON.goldBackpack.upgradeSlotCount::get, ModBlocks.GOLD_BACKPACK));
	public static final RegistryObject<BackpackItem> DIAMOND_BACKPACK = ITEMS.register("diamond_backpack",
			() -> new BackpackItem(Config.COMMON.diamondBackpack.inventorySlotCount::get, Config.COMMON.diamondBackpack.upgradeSlotCount::get, ModBlocks.DIAMOND_BACKPACK));
	public static final RegistryObject<BackpackItem> NETHERITE_BACKPACK = ITEMS.register("netherite_backpack",
			() -> new BackpackItem(Config.COMMON.netheriteBackpack.inventorySlotCount::get, Config.COMMON.netheriteBackpack.upgradeSlotCount::get, ModBlocks.NETHERITE_BACKPACK, new Item.Properties().fireResistant()));
	public static final RegistryObject<PickupUpgradeItem> PICKUP_UPGRADE = ITEMS.register("pickup_upgrade",
			() -> new PickupUpgradeItem(Config.COMMON.pickupUpgrade.filterSlots::get));
	public static final RegistryObject<PickupUpgradeItem> ADVANCED_PICKUP_UPGRADE = ITEMS.register("advanced_pickup_upgrade",
			() -> new PickupUpgradeItem(Config.COMMON.advancedPickupUpgrade.filterSlots::get));
	public static final RegistryObject<FilterUpgradeItem> FILTER_UPGRADE = ITEMS.register("filter_upgrade",
			() -> new FilterUpgradeItem(Config.COMMON.filterUpgrade.filterSlots::get));
	public static final RegistryObject<FilterUpgradeItem> ADVANCED_FILTER_UPGRADE = ITEMS.register("advanced_filter_upgrade",
			() -> new FilterUpgradeItem(Config.COMMON.advancedFilterUpgrade.filterSlots::get));
	public static final RegistryObject<MagnetUpgradeItem> MAGNET_UPGRADE = ITEMS.register("magnet_upgrade",
			() -> new MagnetUpgradeItem(Config.COMMON.magnetUpgrade.magnetRange::get, Config.COMMON.magnetUpgrade.filterSlots::get));
	public static final RegistryObject<MagnetUpgradeItem> ADVANCED_MAGNET_UPGRADE = ITEMS.register("advanced_magnet_upgrade",
			() -> new MagnetUpgradeItem(Config.COMMON.advancedMagnetUpgrade.magnetRange::get, Config.COMMON.advancedMagnetUpgrade.filterSlots::get));
	public static final RegistryObject<FeedingUpgradeItem> FEEDING_UPGRADE = ITEMS.register("feeding_upgrade",
			() -> new FeedingUpgradeItem(Config.COMMON.feedingUpgrade.filterSlots::get));
	public static final RegistryObject<FeedingUpgradeItem> ADVANCED_FEEDING_UPGRADE = ITEMS.register("advanced_feeding_upgrade",
			() -> new FeedingUpgradeItem(Config.COMMON.advancedFeedingUpgrade.filterSlots::get));
	public static final RegistryObject<CompactingUpgradeItem> COMPACTING_UPGRADE = ITEMS.register("compacting_upgrade",
			() -> new CompactingUpgradeItem(false, Config.COMMON.compactingUpgrade.filterSlots::get));
	public static final RegistryObject<CompactingUpgradeItem> ADVANCED_COMPACTING_UPGRADE = ITEMS.register("advanced_compacting_upgrade",
			() -> new CompactingUpgradeItem(true, Config.COMMON.advancedCompactingUpgrade.filterSlots::get));
	public static final RegistryObject<VoidUpgradeItem> VOID_UPGRADE = ITEMS.register("void_upgrade",
			() -> new VoidUpgradeItem(Config.COMMON.voidUpgrade.filterSlots::get));
	public static final RegistryObject<VoidUpgradeItem> ADVANCED_VOID_UPGRADE = ITEMS.register("advanced_void_upgrade",
			() -> new VoidUpgradeItem(Config.COMMON.advancedVoidUpgrade.filterSlots::get));
	public static final RegistryObject<RestockUpgradeItem> RESTOCK_UPGRADE = ITEMS.register("restock_upgrade",
			() -> new RestockUpgradeItem(Config.COMMON.restockUpgrade.filterSlots::get));
	public static final RegistryObject<RestockUpgradeItem> ADVANCED_RESTOCK_UPGRADE = ITEMS.register("advanced_restock_upgrade",
			() -> new RestockUpgradeItem(Config.COMMON.advancedRestockUpgrade.filterSlots::get));
	public static final RegistryObject<DepositUpgradeItem> DEPOSIT_UPGRADE = ITEMS.register("deposit_upgrade",
			() -> new DepositUpgradeItem(Config.COMMON.depositUpgrade.filterSlots::get));
	public static final RegistryObject<DepositUpgradeItem> ADVANCED_DEPOSIT_UPGRADE = ITEMS.register("advanced_deposit_upgrade",
			() -> new DepositUpgradeItem(Config.COMMON.advancedDepositUpgrade.filterSlots::get));
	public static final RegistryObject<RefillUpgradeItem> REFILL_UPGRADE = ITEMS.register("refill_upgrade",
			RefillUpgradeItem::new);
	public static final RegistryObject<InceptionUpgradeItem> INCEPTION_UPGRADE = ITEMS.register("inception_upgrade",
			InceptionUpgradeItem::new);
	public static final RegistryObject<EverlastingUpgradeItem> EVERLASTING_UPGRADE = ITEMS.register("everlasting_upgrade",
			EverlastingUpgradeItem::new);
	public static final RegistryObject<SmeltingUpgradeItem> SMELTING_UPGRADE = ITEMS.register("smelting_upgrade",
			SmeltingUpgradeItem::new);
	public static final RegistryObject<AutoSmeltingUpgradeItem> AUTO_SMELTING_UPGRADE = ITEMS.register("auto_smelting_upgrade",
			AutoSmeltingUpgradeItem::new);
	public static final RegistryObject<CraftingUpgradeItem> CRAFTING_UPGRADE = ITEMS.register("crafting_upgrade",
			CraftingUpgradeItem::new);
	public static final RegistryObject<StonecutterUpgradeItem> STONECUTTER_UPGRADE = ITEMS.register("stonecutter_upgrade",
			StonecutterUpgradeItem::new);
	public static final RegistryObject<StackUpgradeItem> STACK_UPGRADE_TIER_1 = ITEMS.register("stack_upgrade_tier_1", () ->
			new StackUpgradeItem(2));
	public static final RegistryObject<StackUpgradeItem> STACK_UPGRADE_TIER_2 = ITEMS.register("stack_upgrade_tier_2", () ->
			new StackUpgradeItem(4));
	public static final RegistryObject<StackUpgradeItem> STACK_UPGRADE_TIER_3 = ITEMS.register("stack_upgrade_tier_3", () ->
			new StackUpgradeItem(8));
	public static final RegistryObject<StackUpgradeItem> STACK_UPGRADE_TIER_4 = ITEMS.register("stack_upgrade_tier_4", () ->
			new StackUpgradeItem(16));
	public static final RegistryObject<JukeboxUpgradeItem> JUKEBOX_UPGRADE = ITEMS.register("jukebox_upgrade",
			JukeboxUpgradeItem::new);
	public static final RegistryObject<ToolSwapperUpgradeItem> TOOL_SWAPPER_UPGRADE = ITEMS.register("tool_swapper_upgrade",
			() -> new ToolSwapperUpgradeItem(false, false));
	public static final RegistryObject<ToolSwapperUpgradeItem> ADVANCED_TOOL_SWAPPER_UPGRADE = ITEMS.register("advanced_tool_swapper_upgrade",
			() -> new ToolSwapperUpgradeItem(true, true));
	public static final RegistryObject<TankUpgradeItem> TANK_UPGRADE = ITEMS.register("tank_upgrade", TankUpgradeItem::new);
	public static final RegistryObject<BatteryUpgradeItem> BATTERY_UPGRADE = ITEMS.register("battery_upgrade", BatteryUpgradeItem::new);

	public static final RegistryObject<ItemBase> UPGRADE_BASE = ITEMS.register("upgrade_base", () -> new ItemBase(new Item.Properties().stacksTo(16)));

	public static final RegistryObject<ContainerType<BackpackContainer>> BACKPACK_CONTAINER_TYPE = CONTAINERS.register("backpack",
			() -> IForgeContainerType.create(BackpackContainer::fromBuffer));

	public static final RegistryObject<ContainerType<SettingsContainer>> SETTINGS_CONTAINER_TYPE = CONTAINERS.register("settings",
			() -> IForgeContainerType.create(SettingsContainer::fromBuffer));

	public static final RegistryObject<EntityType<EverlastingBackpackItemEntity>> EVERLASTING_BACKPACK_ITEM_ENTITY = ENTITIES.register(
			"everlasting_backpack_item", () -> EntityType.Builder.of(EverlastingBackpackItemEntity::new, EntityClassification.MISC)
					.sized(0.25F, 0.25F).clientTrackingRange(6).updateInterval(20).build("")
	);

	public static void registerHandlers(IEventBus modBus) {
		ITEMS.register(modBus);
		CONTAINERS.register(modBus);
		ENTITIES.register(modBus);
		modBus.addGenericListener(ContainerType.class, ModItems::registerContainers);
		modBus.addGenericListener(IRecipeSerializer.class, ModItems::registerRecipeSerializers);
	}

	private static final UpgradeContainerType<PickupUpgradeWrapper, ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_BASIC_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<PickupUpgradeWrapper, ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_ADVANCED_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<MagnetUpgradeWrapper, ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper>> MAGNET_BASIC_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<MagnetUpgradeWrapper, ContentsFilteredUpgradeContainer<MagnetUpgradeWrapper>> MAGNET_ADVANCED_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> FEEDING_TYPE = new UpgradeContainerType<>(FeedingUpgradeContainer::new);
	private static final UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> ADVANCED_FEEDING_TYPE = new UpgradeContainerType<>(FeedingUpgradeContainer::new);
	private static final UpgradeContainerType<CompactingUpgradeWrapper, CompactingUpgradeContainer> COMPACTING_TYPE = new UpgradeContainerType<>(CompactingUpgradeContainer::new);
	private static final UpgradeContainerType<CompactingUpgradeWrapper, CompactingUpgradeContainer> ADVANCED_COMPACTING_TYPE = new UpgradeContainerType<>(CompactingUpgradeContainer::new);
	private static final UpgradeContainerType<VoidUpgradeWrapper, VoidUpgradeContainer> VOID_TYPE = new UpgradeContainerType<>(VoidUpgradeContainer::new);
	private static final UpgradeContainerType<VoidUpgradeWrapper, VoidUpgradeContainer> ADVANCED_VOID_TYPE = new UpgradeContainerType<>(VoidUpgradeContainer::new);
	private static final UpgradeContainerType<RestockUpgradeWrapper, ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> RESTOCK_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<RestockUpgradeWrapper, ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> ADVANCED_RESTOCK_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	private static final UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> DEPOSIT_TYPE = new UpgradeContainerType<>(DepositUpgradeContainer::new);
	private static final UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> ADVANCED_DEPOSIT_TYPE = new UpgradeContainerType<>(DepositUpgradeContainer::new);
	private static final UpgradeContainerType<RefillUpgradeWrapper, FilteredUpgradeContainer<RefillUpgradeWrapper>> REFILL_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<SmeltingUpgradeWrapper, SmeltingUpgradeContainer> SMELTING_TYPE = new UpgradeContainerType<>(SmeltingUpgradeContainer::new);
	private static final UpgradeContainerType<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeContainer> AUTO_SMELTING_TYPE = new UpgradeContainerType<>(AutoSmeltingUpgradeContainer::new);
	private static final UpgradeContainerType<CraftingUpgradeWrapper, CraftingUpgradeContainer> CRAFTING_TYPE = new UpgradeContainerType<>(CraftingUpgradeContainer::new);
	private static final UpgradeContainerType<InceptionUpgradeWrapper, InceptionUpgradeContainer> INCEPTION_TYPE = new UpgradeContainerType<>(InceptionUpgradeContainer::new);
	private static final UpgradeContainerType<StonecutterUpgradeWrapper, StonecutterUpgradeContainer> STONECUTTER_TYPE = new UpgradeContainerType<>(StonecutterUpgradeContainer::new);
	private static final UpgradeContainerType<JukeboxUpgradeItem.Wrapper, JukeboxUpgradeContainer> JUKEBOX_TYPE = new UpgradeContainerType<>(JukeboxUpgradeContainer::new);
	private static final UpgradeContainerType<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> TOOL_SWAPPER_TYPE = new UpgradeContainerType<>(ToolSwapperUpgradeContainer::new);
	private static final UpgradeContainerType<TankUpgradeWrapper, TankUpgradeContainer> TANK_TYPE = new UpgradeContainerType<>(TankUpgradeContainer::new);
	private static final UpgradeContainerType<BatteryUpgradeWrapper, BatteryUpgradeContainer> BATTERY_TYPE = new UpgradeContainerType<>(BatteryUpgradeContainer::new);

	public static void registerContainers(RegistryEvent.Register<ContainerType<?>> evt) {
		UpgradeContainerRegistry.register(PICKUP_UPGRADE.getId(), PICKUP_BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_PICKUP_UPGRADE.getId(), PICKUP_ADVANCED_TYPE);
		UpgradeContainerRegistry.register(FILTER_UPGRADE.getId(), FilterUpgradeContainer.BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_FILTER_UPGRADE.getId(), FilterUpgradeContainer.ADVANCED_TYPE);
		UpgradeContainerRegistry.register(MAGNET_UPGRADE.getId(), MAGNET_BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_MAGNET_UPGRADE.getId(), MAGNET_ADVANCED_TYPE);
		UpgradeContainerRegistry.register(FEEDING_UPGRADE.getId(), FEEDING_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_FEEDING_UPGRADE.getId(), ADVANCED_FEEDING_TYPE);
		UpgradeContainerRegistry.register(COMPACTING_UPGRADE.getId(), COMPACTING_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_COMPACTING_UPGRADE.getId(), ADVANCED_COMPACTING_TYPE);
		UpgradeContainerRegistry.register(VOID_UPGRADE.getId(), VOID_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_VOID_UPGRADE.getId(), ADVANCED_VOID_TYPE);
		UpgradeContainerRegistry.register(RESTOCK_UPGRADE.getId(), RESTOCK_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_RESTOCK_UPGRADE.getId(), ADVANCED_RESTOCK_TYPE);
		UpgradeContainerRegistry.register(DEPOSIT_UPGRADE.getId(), DEPOSIT_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_DEPOSIT_UPGRADE.getId(), ADVANCED_DEPOSIT_TYPE);
		UpgradeContainerRegistry.register(REFILL_UPGRADE.getId(), REFILL_TYPE);
		UpgradeContainerRegistry.register(SMELTING_UPGRADE.getId(), SMELTING_TYPE);
		UpgradeContainerRegistry.register(AUTO_SMELTING_UPGRADE.getId(), AUTO_SMELTING_TYPE);
		UpgradeContainerRegistry.register(CRAFTING_UPGRADE.getId(), CRAFTING_TYPE);
		UpgradeContainerRegistry.register(INCEPTION_UPGRADE.getId(), INCEPTION_TYPE);
		UpgradeContainerRegistry.register(STONECUTTER_UPGRADE.getId(), STONECUTTER_TYPE);
		UpgradeContainerRegistry.register(JUKEBOX_UPGRADE.getId(), JUKEBOX_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_TOOL_SWAPPER_UPGRADE.getId(), TOOL_SWAPPER_TYPE);
		UpgradeContainerRegistry.register(TANK_UPGRADE.getId(), TANK_TYPE);
		UpgradeContainerRegistry.register(BATTERY_UPGRADE.getId(), BATTERY_TYPE);

		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			ScreenManager.register(BACKPACK_CONTAINER_TYPE.get(), BackpackScreen::constructScreen);
			ScreenManager.register(SETTINGS_CONTAINER_TYPE.get(), SettingsScreen::constructScreen);

			UpgradeGuiManager.registerTab(PICKUP_BASIC_TYPE, PickupUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(PICKUP_ADVANCED_TYPE, PickupUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(FilterUpgradeContainer.BASIC_TYPE, FilterUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(FilterUpgradeContainer.ADVANCED_TYPE, FilterUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(MAGNET_BASIC_TYPE, MagnetUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(MAGNET_ADVANCED_TYPE, MagnetUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(FEEDING_TYPE, FeedingUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(ADVANCED_FEEDING_TYPE, FeedingUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(COMPACTING_TYPE, CompactingUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(ADVANCED_COMPACTING_TYPE, CompactingUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(VOID_TYPE, VoidUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(ADVANCED_VOID_TYPE, VoidUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(RESTOCK_TYPE, RestockUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(ADVANCED_RESTOCK_TYPE, RestockUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(DEPOSIT_TYPE, DepositUpgradeTab.Basic::new);
			UpgradeGuiManager.registerTab(ADVANCED_DEPOSIT_TYPE, DepositUpgradeTab.Advanced::new);
			UpgradeGuiManager.registerTab(REFILL_TYPE, RefillUpgradeTab::new);
			UpgradeGuiManager.registerTab(SMELTING_TYPE, SmeltingUpgradeTab::new);
			UpgradeGuiManager.registerTab(AUTO_SMELTING_TYPE, AutoSmeltingUpgradeTab::new);
			UpgradeGuiManager.registerTab(CRAFTING_TYPE, CraftingUpgradeTab::new);
			UpgradeGuiManager.registerTab(INCEPTION_TYPE, InceptionUpgradeTab::new);
			UpgradeGuiManager.registerTab(STONECUTTER_TYPE, StonecutterUpgradeTab::new);
			UpgradeGuiManager.registerTab(JUKEBOX_TYPE, JukeboxUpgradeTab::new);
			UpgradeGuiManager.registerTab(TOOL_SWAPPER_TYPE, ToolSwapperUpgradeTab::new);
			UpgradeGuiManager.registerTab(TANK_TYPE, TankUpgradeTab::new);
			UpgradeGuiManager.registerTab(BATTERY_TYPE, BatteryUpgradeTab::new);
			UpgradeGuiManager.registerInventoryPart(TANK_TYPE, TankInventoryPart::new);
			UpgradeGuiManager.registerInventoryPart(BATTERY_TYPE, BatteryInventoryPart::new);
		});
	}

	public static void registerRecipeSerializers(RegistryEvent.Register<IRecipeSerializer<?>> evt) {
		CraftingHelper.register(ItemEnabledCondition.Serializer.INSTANCE);

		evt.getRegistry().register(BackpackUpgradeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_upgrade"));
		evt.getRegistry().register(SmithingBackpackUpgradeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "smithing_backpack_upgrade"));
		evt.getRegistry().register(UpgradeNextTierRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "upgrade_next_tier"));
		evt.getRegistry().register(BackpackDyeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_dye"));
	}

	public static void registerDispenseBehavior() {
		DispenserBlock.registerBehavior(BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(IRON_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(GOLD_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(DIAMOND_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(NETHERITE_BACKPACK.get(), new BackpackDispenseBehavior());
	}

	private static class BackpackDispenseBehavior extends OptionalDispenseBehavior {
		@Override
		protected ItemStack execute(IBlockSource source, ItemStack stack) {
			setSuccess(false);
			Item item = stack.getItem();
			if (item instanceof BackpackItem) {
				Direction dispenserDirection = source.getBlockState().getValue(DispenserBlock.FACING);
				BlockPos blockpos = source.getPos().relative(dispenserDirection);
				Direction against = source.getLevel().isEmptyBlock(blockpos.below()) ? dispenserDirection.getOpposite() : Direction.UP;

				setSuccess(((BackpackItem) item).tryPlace(null, dispenserDirection.getOpposite(), new DirectionalPlaceContext(source.getLevel(), blockpos, dispenserDirection, stack, against)).consumesAction());
			}

			return stack;
		}
	}
}
