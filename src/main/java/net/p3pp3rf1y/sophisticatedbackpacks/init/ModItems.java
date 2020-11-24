package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.client.gui.ScreenManager;
import net.minecraft.inventory.container.ContainerType;
import net.minecraft.item.Item;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.common.extensions.IForgeContainerType;
import net.minecraftforge.event.RegistryEvent;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.RegistryObject;
import net.minecraftforge.registries.DeferredRegister;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTabManager;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackSingleDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackTwoDyesRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.items.ScreenProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.compacting.CompactingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.feeding.FeedingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.voiding.VoidUpgradeWrapper;

public class ModItems {
	private ModItems() {}

	private static final DeferredRegister<Item> ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister<ContainerType<?>> CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, SophisticatedBackpacks.MOD_ID);

	public static final RegistryObject<BackpackItem> BACKPACK = ITEMS.register("backpack",
			() -> new BackpackItem(27, 1, ModBlocks.BACKPACK));
	public static final RegistryObject<BackpackItem> IRON_BACKPACK = ITEMS.register("iron_backpack",
			() -> new BackpackItem(54, 2, ModBlocks.IRON_BACKPACK));
	public static final RegistryObject<BackpackItem> GOLD_BACKPACK = ITEMS.register("gold_backpack",
			() -> new BackpackItem(81, 3, new ScreenProperties().setTextureSize(512), ModBlocks.GOLD_BACKPACK));
	public static final RegistryObject<BackpackItem> DIAMOND_BACKPACK = ITEMS.register("diamond_backpack",
			() -> new BackpackItem(108, 5, new ScreenProperties().setSlotsOnLine(12).setPlayerInventoryYOffset(27).setTextureSize(512), ModBlocks.DIAMOND_BACKPACK));
	public static final RegistryObject<PickupUpgradeItem> PICKUP_UPGRADE = ITEMS.register("pickup_upgrade", PickupUpgradeItem::new);
	public static final RegistryObject<PickupUpgradeItem> ADVANCED_PICKUP_UPGRADE = ITEMS.register("advanced_pickup_upgrade", () -> new PickupUpgradeItem(16));
	public static final RegistryObject<FilterUpgradeItem> FILTER_UPGRADE = ITEMS.register("filter_upgrade", FilterUpgradeItem::new);
	public static final RegistryObject<FilterUpgradeItem> ADVANCED_FILTER_UPGRADE = ITEMS.register("advanced_filter_upgrade", () -> new FilterUpgradeItem(16));
	public static final RegistryObject<MagnetUpgradeItem> MAGNET_UPGRADE = ITEMS.register("magnet_upgrade", MagnetUpgradeItem::new);
	public static final RegistryObject<MagnetUpgradeItem> ADVANCED_MAGNET_UPGRADE = ITEMS.register("advanced_magnet_upgrade", () -> new MagnetUpgradeItem(5, 16));
	public static final RegistryObject<FeedingUpgradeItem> FEEDING_UPGRADE = ITEMS.register("feeding_upgrade", FeedingUpgradeItem::new);
	public static final RegistryObject<CompactingUpgradeItem> COMPACTING_UPGRADE = ITEMS.register("compacting_upgrade", CompactingUpgradeItem::new);
	public static final RegistryObject<CompactingUpgradeItem> ADVANCED_COMPACTING_UPGRADE = ITEMS.register("advanced_compacting_upgrade", () -> new CompactingUpgradeItem(true, 16));
	public static final RegistryObject<VoidUpgradeItem> VOID_UPGRADE = ITEMS.register("void_upgrade", VoidUpgradeItem::new);
	public static final RegistryObject<VoidUpgradeItem> ADVANCED_VOID_UPGRADE = ITEMS.register("advanced_void_upgrade", () -> new VoidUpgradeItem(16));
	public static final RegistryObject<ItemBase> UPGRADE_BASE = ITEMS.register("upgrade_base", () -> new ItemBase(new Item.Properties().maxStackSize(16)));

	public static final RegistryObject<ContainerType<BackpackContainer>> BACKPACK_ITEM_CONTAINER_TYPE = CONTAINERS.register("backpack",
			() -> IForgeContainerType.create(BackpackContainer::fromBufferItem));
	public static final RegistryObject<ContainerType<BackpackContainer>> BACKPACK_BLOCK_CONTAINER_TYPE = CONTAINERS.register("backpack_block",
			() -> IForgeContainerType.create(BackpackContainer::fromBufferBlock));

	public static void registerHandlers(IEventBus modBus) {
		ITEMS.register(modBus);
		CONTAINERS.register(modBus);
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
		});

	}

	public static void registerRecipeSerializers(RegistryEvent.Register<IRecipeSerializer<?>> evt) {
		evt.getRegistry().register(BackpackUpgradeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_upgrade"));
		evt.getRegistry().register(UpgradeNextTierRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "upgrade_next_tier"));
		evt.getRegistry().register(BackpackSingleDyeRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_single_dye"));
		evt.getRegistry().register(BackpackTwoDyesRecipe.SERIALIZER.setRegistryName(SophisticatedBackpacks.MOD_ID, "backpack_two_dyes"));
	}
}
