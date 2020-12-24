package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.client.gui.ScreenManager;
import net.minecraft.entity.EntityClassification;
import net.minecraft.entity.EntityType;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.crafting.IRecipeSerializer;
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
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTabManager;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackSingleDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackTwoDyesRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.ItemEnabledCondition;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeTab;
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
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeWrapper;

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
			FeedingUpgradeItem::new);
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
	public static final RegistryObject<ItemBase> UPGRADE_BASE = ITEMS.register("upgrade_base", () -> new ItemBase(new Item.Properties().maxStackSize(16)));

	public static final RegistryObject<ContainerType<BackpackContainer>> BACKPACK_ITEM_CONTAINER_TYPE = CONTAINERS.register("backpack",
			() -> IForgeContainerType.create(BackpackContainer::fromBufferItem));
	public static final RegistryObject<ContainerType<BackpackContainer>> BACKPACK_BLOCK_CONTAINER_TYPE = CONTAINERS.register("backpack_block",
			() -> IForgeContainerType.create(BackpackContainer::fromBufferBlock));

	public static final RegistryObject<EntityType<EverlastingBackpackItemEntity>> EVERLASTING_BACKPACK_ITEM_ENTITY = ENTITIES.register(
			"everlasting_backpack_item", () -> EntityType.Builder.create(EverlastingBackpackItemEntity::new, EntityClassification.MISC)
					.size(0.25F, 0.25F).trackingRange(6).func_233608_b_(20).build("")
	);

	public static void registerHandlers(IEventBus modBus) {
		ITEMS.register(modBus);
		CONTAINERS.register(modBus);
		ENTITIES.register(modBus);
		modBus.addGenericListener(ContainerType.class, ModItems::registerContainers);
		modBus.addGenericListener(IRecipeSerializer.class, ModItems::registerRecipeSerializers);
	}

	private static final UpgradeContainerType<PickupUpgradeWrapper, FilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_BASIC_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<PickupUpgradeWrapper, FilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_ADVANCED_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<MagnetUpgradeWrapper, FilteredUpgradeContainer<MagnetUpgradeWrapper>> MAGNET_BASIC_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<MagnetUpgradeWrapper, FilteredUpgradeContainer<MagnetUpgradeWrapper>> MAGNET_ADVANCED_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<FeedingUpgradeWrapper, FilteredUpgradeContainer<FeedingUpgradeWrapper>> FEEDING_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<CompactingUpgradeWrapper, FilteredUpgradeContainer<CompactingUpgradeWrapper>> COMPACTING_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<CompactingUpgradeWrapper, FilteredUpgradeContainer<CompactingUpgradeWrapper>> ADVANCED_COMPACTING_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<VoidUpgradeWrapper, FilteredUpgradeContainer<VoidUpgradeWrapper>> VOID_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<VoidUpgradeWrapper, FilteredUpgradeContainer<VoidUpgradeWrapper>> ADVANCED_VOID_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<RestockUpgradeWrapper, FilteredUpgradeContainer<RestockUpgradeWrapper>> RESTOCK_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<RestockUpgradeWrapper, FilteredUpgradeContainer<RestockUpgradeWrapper>> ADVANCED_RESTOCK_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<DepositUpgradeWrapper, FilteredUpgradeContainer<DepositUpgradeWrapper>> DEPOSIT_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<DepositUpgradeWrapper, FilteredUpgradeContainer<DepositUpgradeWrapper>> ADVANCED_DEPOSIT_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<RefillUpgradeWrapper, FilteredUpgradeContainer<RefillUpgradeWrapper>> REFILL_TYPE = new UpgradeContainerType<>(FilteredUpgradeContainer::new);
	private static final UpgradeContainerType<SmeltingUpgradeWrapper, SmeltingUpgradeContainer> SMELTING_TYPE = new UpgradeContainerType<>(SmeltingUpgradeContainer::new);
	private static final UpgradeContainerType<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeContainer> AUTO_SMELTING_TYPE = new UpgradeContainerType<>(AutoSmeltingUpgradeContainer::new);
	private static final UpgradeContainerType<CraftingUpgradeWrapper, CraftingUpgradeContainer> CRAFTING_TYPE = new UpgradeContainerType<>(CraftingUpgradeContainer::new);
	private static final UpgradeContainerType<InceptionUpgradeItem.Wrapper, InceptionUpgradeContainer> INCEPTION_TYPE = new UpgradeContainerType<>(InceptionUpgradeContainer::new);

	public static void registerContainers(RegistryEvent.Register<ContainerType<?>> evt) {
		UpgradeContainerRegistry.register(PICKUP_UPGRADE.getId(), PICKUP_BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_PICKUP_UPGRADE.getId(), PICKUP_ADVANCED_TYPE);
		UpgradeContainerRegistry.register(FILTER_UPGRADE.getId(), FilterUpgradeContainer.BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_FILTER_UPGRADE.getId(), FilterUpgradeContainer.ADVANCED_TYPE);
		UpgradeContainerRegistry.register(MAGNET_UPGRADE.getId(), MAGNET_BASIC_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_MAGNET_UPGRADE.getId(), MAGNET_ADVANCED_TYPE);
		UpgradeContainerRegistry.register(FEEDING_UPGRADE.getId(), FEEDING_TYPE);
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

		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			ScreenManager.registerFactory(BACKPACK_ITEM_CONTAINER_TYPE.get(), BackpackScreen::new);
			ScreenManager.registerFactory(BACKPACK_BLOCK_CONTAINER_TYPE.get(), BackpackScreen::new);

			UpgradeSettingsTabManager.register(PICKUP_BASIC_TYPE, PickupUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(PICKUP_ADVANCED_TYPE, PickupUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(FilterUpgradeContainer.BASIC_TYPE, FilterUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(FilterUpgradeContainer.ADVANCED_TYPE, FilterUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(MAGNET_BASIC_TYPE, MagnetUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(MAGNET_ADVANCED_TYPE, MagnetUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(FEEDING_TYPE, FeedingUpgradeTab::new);
			UpgradeSettingsTabManager.register(COMPACTING_TYPE, CompactingUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(ADVANCED_COMPACTING_TYPE, CompactingUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(VOID_TYPE, VoidUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(ADVANCED_VOID_TYPE, VoidUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(RESTOCK_TYPE, RestockUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(ADVANCED_RESTOCK_TYPE, RestockUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(DEPOSIT_TYPE, DepositUpgradeTab.Basic::new);
			UpgradeSettingsTabManager.register(ADVANCED_DEPOSIT_TYPE, DepositUpgradeTab.Advanced::new);
			UpgradeSettingsTabManager.register(REFILL_TYPE, RefillUpgradeTab::new);
			UpgradeSettingsTabManager.register(SMELTING_TYPE, SmeltingUpgradeTab::new);
			UpgradeSettingsTabManager.register(AUTO_SMELTING_TYPE, AutoSmeltingUpgradeTab::new);
			UpgradeSettingsTabManager.register(CRAFTING_TYPE, CraftingUpgradeTab::new);
			UpgradeSettingsTabManager.register(INCEPTION_TYPE, InceptionUpgradeTab::new);
		});
	}

	public static void registerRecipeSerializers(RegistryEvent.Register<IRecipeSerializer<?>> evt) {
		CraftingHelper.register(ItemEnabledCondition.Serializer.INSTANCE);

		evt.getRegistry().register(BackpackUpgradeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_upgrade"));
		evt.getRegistry().register(UpgradeNextTierRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "upgrade_next_tier"));
		evt.getRegistry().register(BackpackSingleDyeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_single_dye"));
		evt.getRegistry().register(BackpackTwoDyesRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_two_dyes"));
	}
}
