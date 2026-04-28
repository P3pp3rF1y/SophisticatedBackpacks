package net.p3pp3rf1y.sophisticatedbackpacks.init;

import com.mojang.serialization.MapCodec;
import net.minecraft.core.BlockPos;
import net.minecraft.core.Direction;
import net.minecraft.core.cauldron.CauldronInteraction;
import net.minecraft.core.cauldron.CauldronInteractions;
import net.minecraft.core.dispenser.BlockSource;
import net.minecraft.core.dispenser.OptionalDispenseItemBehavior;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.tags.TagKey;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.MobCategory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.MenuType;
import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.context.DirectionalPlaceContext;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.DispenserBlock;
import net.minecraft.world.level.block.LayeredCauldronBlock;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.level.storage.loot.functions.LootItemFunction;
import net.minecraft.world.level.storage.loot.predicates.LootItemCondition;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.fml.loading.FMLEnvironment;
import net.neoforged.neoforge.attachment.AttachmentType;
import net.neoforged.neoforge.capabilities.Capabilities;
import net.neoforged.neoforge.capabilities.RegisterCapabilitiesEvent;
import net.neoforged.neoforge.common.extensions.IMenuTypeExtension;
import net.neoforged.neoforge.common.loot.IGlobalLootModifier;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.registries.DeferredRegister;
import net.neoforged.neoforge.registries.NeoForgeRegistries;
import net.neoforged.neoforge.registries.RegisterEvent;
import net.neoforged.neoforge.transfer.EmptyResourceHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackSettingsContainerMenu;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BasicBackpackRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.data.BackpackLootEnabledCondition;
import net.p3pp3rf1y.sophisticatedbackpacks.data.BackpackLootModifierProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.data.CopyBackpackDataFunction;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil.AnvilUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil.AnvilUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil.AnvilUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingBackpackItemEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting.EverlastingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing.SmithingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing.SmithingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing.SmithingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.alchemy.AlchemyUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.alchemy.AlchemyUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.alchemy.AlchemyUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.*;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.FeedingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.FeedingUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.FeedingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.filter.FilterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.infinity.InfinityUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pickup.PickupUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pickup.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pump.PumpUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pump.PumpUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pump.PumpUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stack.StackUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter.StonecutterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter.StonecutterUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.ItemBase;

import java.util.function.Supplier;

public class ModItems {
	private ModItems() {
	}

	public static final DeferredRegister.Items ITEMS = DeferredRegister.createItems(SophisticatedBackpacks.MOD_ID);
	public static final DeferredRegister<CreativeModeTab> CREATIVE_MODE_TABS = DeferredRegister.create(Registries.CREATIVE_MODE_TAB.identifier(), SophisticatedBackpacks.MOD_ID);
	public static final DeferredRegister<MapCodec<? extends LootItemFunction>> LOOT_FUNCTION_TYPES = DeferredRegister.create(Registries.LOOT_FUNCTION_TYPE.identifier(), SophisticatedBackpacks.MOD_ID);
	public static final DeferredRegister<MapCodec<? extends LootItemCondition>> LOOT_CONDITION_TYPES = DeferredRegister.create(Registries.LOOT_CONDITION_TYPE.identifier(), SophisticatedBackpacks.MOD_ID);
	public static final DeferredRegister<MapCodec<? extends IGlobalLootModifier>> LOOT_MODIFIERS = DeferredRegister.create(NeoForgeRegistries.GLOBAL_LOOT_MODIFIER_SERIALIZERS, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister<MenuType<?>> MENU_TYPES = DeferredRegister.create(BuiltInRegistries.MENU, SophisticatedBackpacks.MOD_ID);
	private static final DeferredRegister.Entities ENTITY_TYPES = DeferredRegister.createEntities(SophisticatedBackpacks.MOD_ID);

	public static final DeferredHolder<Item, BackpackItem> BACKPACK = ITEMS.registerItem("backpack",
			properties -> new BackpackItem(Config.SERVER.leatherBackpack.inventorySlotCount::get, Config.SERVER.leatherBackpack.upgradeSlotCount::get, ModBlocks.BACKPACK, properties));
	public static final DeferredHolder<Item, BackpackItem> COPPER_BACKPACK = ITEMS.registerItem("copper_backpack",
			properties -> new BackpackItem(Config.SERVER.copperBackpack.inventorySlotCount::get, Config.SERVER.copperBackpack.upgradeSlotCount::get, ModBlocks.COPPER_BACKPACK, properties));
	public static final DeferredHolder<Item, BackpackItem> IRON_BACKPACK = ITEMS.registerItem("iron_backpack",
			properties -> new BackpackItem(Config.SERVER.ironBackpack.inventorySlotCount::get, Config.SERVER.ironBackpack.upgradeSlotCount::get, ModBlocks.IRON_BACKPACK, properties));
	public static final DeferredHolder<Item, BackpackItem> GOLD_BACKPACK = ITEMS.registerItem("gold_backpack",
			properties -> new BackpackItem(Config.SERVER.goldBackpack.inventorySlotCount::get, Config.SERVER.goldBackpack.upgradeSlotCount::get, ModBlocks.GOLD_BACKPACK, properties));
	public static final DeferredHolder<Item, BackpackItem> DIAMOND_BACKPACK = ITEMS.registerItem("diamond_backpack",
			properties -> new BackpackItem(Config.SERVER.diamondBackpack.inventorySlotCount::get, Config.SERVER.diamondBackpack.upgradeSlotCount::get, ModBlocks.DIAMOND_BACKPACK, properties));
	public static final DeferredHolder<Item, BackpackItem> NETHERITE_BACKPACK = ITEMS.registerItem("netherite_backpack",
			properties -> new BackpackItem(Config.SERVER.netheriteBackpack.inventorySlotCount::get, Config.SERVER.netheriteBackpack.upgradeSlotCount::get, ModBlocks.NETHERITE_BACKPACK, Item.Properties::fireResistant, properties));

	public static final Identifier BACKPACK_UPGRADE_TAG_NAME = Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "upgrade");

	public static final TagKey<Item> BACKPACK_UPGRADE_TAG = TagKey.create(Registries.ITEM, BACKPACK_UPGRADE_TAG_NAME);

	public static final DeferredHolder<Item, PickupUpgradeItem> PICKUP_UPGRADE = ITEMS.registerItem("pickup_upgrade",
			properties -> new PickupUpgradeItem(Config.SERVER.pickupUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, PickupUpgradeItem> ADVANCED_PICKUP_UPGRADE = ITEMS.registerItem("advanced_pickup_upgrade",
			properties -> new PickupUpgradeItem(Config.SERVER.advancedPickupUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, FilterUpgradeItem> FILTER_UPGRADE = ITEMS.registerItem("filter_upgrade",
			properties -> new FilterUpgradeItem(Config.SERVER.filterUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, FilterUpgradeItem> ADVANCED_FILTER_UPGRADE = ITEMS.registerItem("advanced_filter_upgrade",
			properties -> new FilterUpgradeItem(Config.SERVER.advancedFilterUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, MagnetUpgradeItem> MAGNET_UPGRADE = ITEMS.registerItem("magnet_upgrade",
			properties -> new MagnetUpgradeItem(Config.SERVER.magnetUpgrade.magnetRange::get, Config.SERVER.magnetUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, MagnetUpgradeItem> ADVANCED_MAGNET_UPGRADE = ITEMS.registerItem("advanced_magnet_upgrade",
			properties -> new MagnetUpgradeItem(Config.SERVER.advancedMagnetUpgrade.magnetRange::get, Config.SERVER.advancedMagnetUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, FeedingUpgradeItem> FEEDING_UPGRADE = ITEMS.registerItem("feeding_upgrade",
			properties -> new FeedingUpgradeItem(Config.SERVER.feedingUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, FeedingUpgradeItem> ADVANCED_FEEDING_UPGRADE = ITEMS.registerItem("advanced_feeding_upgrade",
			properties -> new FeedingUpgradeItem(Config.SERVER.advancedFeedingUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, CompactingUpgradeItem> COMPACTING_UPGRADE = ITEMS.registerItem("compacting_upgrade",
			properties -> new CompactingUpgradeItem(false, Config.SERVER.compactingUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, CompactingUpgradeItem> ADVANCED_COMPACTING_UPGRADE = ITEMS.registerItem("advanced_compacting_upgrade",
			properties -> new CompactingUpgradeItem(true, Config.SERVER.advancedCompactingUpgrade.filterSlots::get, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, VoidUpgradeItem> VOID_UPGRADE = ITEMS.registerItem("void_upgrade",
			properties -> new VoidUpgradeItem(Config.SERVER.voidUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, VoidUpgradeItem> ADVANCED_VOID_UPGRADE = ITEMS.registerItem("advanced_void_upgrade",
			properties -> new VoidUpgradeItem(Config.SERVER.advancedVoidUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, RestockUpgradeItem> RESTOCK_UPGRADE = ITEMS.registerItem("restock_upgrade",
			properties -> new RestockUpgradeItem(Config.SERVER.restockUpgrade.filterSlots::get, properties));
	public static final DeferredHolder<Item, RestockUpgradeItem> ADVANCED_RESTOCK_UPGRADE = ITEMS.registerItem("advanced_restock_upgrade",
			properties -> new RestockUpgradeItem(Config.SERVER.advancedRestockUpgrade.filterSlots::get, properties));
	public static final DeferredHolder<Item, DepositUpgradeItem> DEPOSIT_UPGRADE = ITEMS.registerItem("deposit_upgrade",
			properties -> new DepositUpgradeItem(Config.SERVER.depositUpgrade.filterSlots::get, properties));
	public static final DeferredHolder<Item, DepositUpgradeItem> ADVANCED_DEPOSIT_UPGRADE = ITEMS.registerItem("advanced_deposit_upgrade",
			properties -> new DepositUpgradeItem(Config.SERVER.advancedDepositUpgrade.filterSlots::get, properties));
	public static final DeferredHolder<Item, RefillUpgradeItem> REFILL_UPGRADE = ITEMS.registerItem("refill_upgrade",
			properties -> new RefillUpgradeItem(Config.SERVER.refillUpgrade.filterSlots::get, false, false, properties));
	public static final DeferredHolder<Item, RefillUpgradeItem> ADVANCED_REFILL_UPGRADE = ITEMS.registerItem("advanced_refill_upgrade",
			properties -> new RefillUpgradeItem(Config.SERVER.advancedRefillUpgrade.filterSlots::get, true, true, properties));
	public static final DeferredHolder<Item, InceptionUpgradeItem> INCEPTION_UPGRADE = ITEMS.registerItem("inception_upgrade",
			InceptionUpgradeItem::new);
	public static final DeferredHolder<Item, EverlastingUpgradeItem> EVERLASTING_UPGRADE = ITEMS.registerItem("everlasting_upgrade",
			EverlastingUpgradeItem::new);
	public static final DeferredHolder<Item, SmeltingUpgradeItem> SMELTING_UPGRADE = ITEMS.registerItem("smelting_upgrade",
			properties -> new SmeltingUpgradeItem(Config.SERVER.smeltingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, AutoSmeltingUpgradeItem> AUTO_SMELTING_UPGRADE = ITEMS.registerItem("auto_smelting_upgrade",
			properties -> new AutoSmeltingUpgradeItem(Config.SERVER.autoSmeltingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, SmokingUpgradeItem> SMOKING_UPGRADE = ITEMS.registerItem("smoking_upgrade",
			properties -> new SmokingUpgradeItem(Config.SERVER.smokingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, AutoSmokingUpgradeItem> AUTO_SMOKING_UPGRADE = ITEMS.registerItem("auto_smoking_upgrade",
			properties -> new AutoSmokingUpgradeItem(Config.SERVER.autoSmokingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, BlastingUpgradeItem> BLASTING_UPGRADE = ITEMS.registerItem("blasting_upgrade",
			properties -> new BlastingUpgradeItem(Config.SERVER.blastingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, AutoBlastingUpgradeItem> AUTO_BLASTING_UPGRADE = ITEMS.registerItem("auto_blasting_upgrade",
			properties -> new AutoBlastingUpgradeItem(Config.SERVER.autoBlastingUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, CraftingUpgradeItem> CRAFTING_UPGRADE = ITEMS.registerItem("crafting_upgrade", properties -> new CraftingUpgradeItem(Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StonecutterUpgradeItem> STONECUTTER_UPGRADE = ITEMS.registerItem("stonecutter_upgrade", properties -> new StonecutterUpgradeItem(Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_STARTER_TIER = ITEMS.registerItem("stack_upgrade_starter_tier", properties ->
			new StackUpgradeItem(1.5D, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_TIER_1 = ITEMS.registerItem("stack_upgrade_tier_1", properties ->
			new StackUpgradeItem(2, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_TIER_2 = ITEMS.registerItem("stack_upgrade_tier_2", properties ->
			new StackUpgradeItem(4, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_TIER_3 = ITEMS.registerItem("stack_upgrade_tier_3", properties ->
			new StackUpgradeItem(8, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_TIER_4 = ITEMS.registerItem("stack_upgrade_tier_4", properties ->
			new StackUpgradeItem(16, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_DOWNGRADE_TIER_1 = ITEMS.registerItem("stack_downgrade_tier_1", properties ->
			new StackUpgradeItem(0.125, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_DOWNGRADE_TIER_2 = ITEMS.registerItem("stack_downgrade_tier_2", properties ->
			new StackUpgradeItem(0.0625, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_DOWNGRADE_TIER_3 = ITEMS.registerItem("stack_downgrade_tier_3", properties ->
			new StackUpgradeItem(0.03125, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, StackUpgradeItem> STACK_UPGRADE_OMEGA_TIER = ITEMS.registerItem("stack_upgrade_omega_tier", properties ->
			new StackUpgradeItem(Integer.MAX_VALUE, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, JukeboxUpgradeItem> JUKEBOX_UPGRADE = ITEMS.registerItem("jukebox_upgrade", properties -> new JukeboxUpgradeItem(Config.SERVER.maxUpgradesPerStorage, () -> 1, () -> 1, properties));
	public static final DeferredHolder<Item, JukeboxUpgradeItem> ADVANCED_JUKEBOX_UPGRADE = ITEMS.registerItem("advanced_jukebox_upgrade", properties -> new JukeboxUpgradeItem(Config.SERVER.maxUpgradesPerStorage, Config.SERVER.advancedJukeboxUpgrade.numberOfSlots, Config.SERVER.advancedJukeboxUpgrade.slotsInRow, properties));
	public static final DeferredHolder<Item, ToolSwapperUpgradeItem> TOOL_SWAPPER_UPGRADE = ITEMS.registerItem("tool_swapper_upgrade",
			properties -> new ToolSwapperUpgradeItem(false, false, properties));
	public static final DeferredHolder<Item, ToolSwapperUpgradeItem> ADVANCED_TOOL_SWAPPER_UPGRADE = ITEMS.registerItem("advanced_tool_swapper_upgrade",
			properties -> new ToolSwapperUpgradeItem(true, true, properties));
	public static final DeferredHolder<Item, TankUpgradeItem> TANK_UPGRADE = ITEMS.registerItem("tank_upgrade", properties -> new TankUpgradeItem(Config.SERVER.tankUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, BatteryUpgradeItem> BATTERY_UPGRADE = ITEMS.registerItem("battery_upgrade", properties -> new BatteryUpgradeItem(Config.SERVER.batteryUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, PumpUpgradeItem> PUMP_UPGRADE = ITEMS.registerItem("pump_upgrade", properties -> new PumpUpgradeItem(false, false, true, Config.SERVER.pumpUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, PumpUpgradeItem> ADVANCED_PUMP_UPGRADE = ITEMS.registerItem("advanced_pump_upgrade", properties -> new PumpUpgradeItem(true, false, true, Config.SERVER.pumpUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, XpPumpUpgradeItem> XP_PUMP_UPGRADE = ITEMS.registerItem("xp_pump_upgrade", properties -> new XpPumpUpgradeItem(Config.SERVER.xpPumpUpgrade, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, AnvilUpgradeItem> ANVIL_UPGRADE = ITEMS.registerItem("anvil_upgrade", AnvilUpgradeItem::new);
	public static final DeferredHolder<Item, SmithingUpgradeItem> SMITHING_UPGRADE = ITEMS.registerItem("smithing_upgrade", SmithingUpgradeItem::new);
	public static final DeferredHolder<Item, InfinityUpgradeItem> INFINITY_UPGRADE = ITEMS.registerItem("infinity_upgrade", properties -> new InfinityUpgradeItem(Config.SERVER.maxUpgradesPerStorage, true, properties));
	public static final DeferredHolder<Item, InfinityUpgradeItem> SURVIVAL_INFINITY_UPGRADE = ITEMS.registerItem("survival_infinity_upgrade", properties -> new InfinityUpgradeItem(Config.SERVER.maxUpgradesPerStorage, false, properties));
	public static final DeferredHolder<Item, AlchemyUpgradeItem> ALCHEMY_UPGRADE = ITEMS.registerItem("alchemy_upgrade", properties -> new AlchemyUpgradeItem(Config.SERVER.alchemyUpgrade.filterSlots, Config.SERVER.maxUpgradesPerStorage, properties));
	public static final DeferredHolder<Item, AlchemyUpgradeItem> ADVANCED_ALCHEMY_UPGRADE = ITEMS.registerItem("advanced_alchemy_upgrade", properties -> new AlchemyUpgradeItem(Config.SERVER.advancedAlchemyUpgrade.filterSlots, Config.SERVER.maxUpgradesPerStorage, properties));

	public static final Supplier<ItemBase> UPGRADE_BASE = ITEMS.registerItem("upgrade_base", properties -> new ItemBase(properties.stacksTo(16)));

	public static final Supplier<CreativeModeTab> CREATIVE_TAB = CREATIVE_MODE_TABS.register("main", () ->
			CreativeModeTab.builder().icon(() -> new ItemStack(ModItems.BACKPACK.get()))
					.title(Component.translatable("itemGroup.sophisticatedbackpacks"))
					.displayItems((featureFlags, output) -> ITEMS.getEntries().stream().filter(i -> i.get() instanceof ItemBase).forEach(i -> ((ItemBase) i.get()).addCreativeTabItems(output::accept)))
					.build());

	public static final Supplier<MenuType<BackpackContainer>> BACKPACK_CONTAINER_TYPE = MENU_TYPES.register("backpack",
			() -> IMenuTypeExtension.create(BackpackContainer::fromBuffer));

	public static final Supplier<MenuType<BackpackSettingsContainerMenu>> SETTINGS_CONTAINER_TYPE = MENU_TYPES.register("settings",
			() -> IMenuTypeExtension.create(BackpackSettingsContainerMenu::fromBuffer));

	public static final Supplier<EntityType<EverlastingBackpackItemEntity>> EVERLASTING_BACKPACK_ITEM_ENTITY = ENTITY_TYPES.registerEntityType(
			"everlasting_backpack_item", EverlastingBackpackItemEntity::new, MobCategory.MISC, builder -> builder.sized(0.25F, 0.25F).clientTrackingRange(6).updateInterval(20)
	);

	private static final DeferredRegister<RecipeSerializer<?>> RECIPE_SERIALIZERS = DeferredRegister.create(BuiltInRegistries.RECIPE_SERIALIZER, SophisticatedBackpacks.MOD_ID);

	public static final Supplier<RecipeSerializer<BackpackDyeRecipe>> BACKPACK_DYE_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register("backpack_dye", () -> BackpackDyeRecipe.SERIALIZER);
	public static final Supplier<RecipeSerializer<BackpackUpgradeRecipe>> BACKPACK_UPGRADE_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register("backpack_upgrade", () -> BackpackUpgradeRecipe.SERIALIZER);
	public static final Supplier<RecipeSerializer<SmithingBackpackUpgradeRecipe>> SMITHING_BACKPACK_UPGRADE_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register("smithing_backpack_upgrade", () -> SmithingBackpackUpgradeRecipe.SERIALIZER);
	public static final Supplier<RecipeSerializer<BasicBackpackRecipe>> BASIC_BACKPACK_RECIPE_SERIALIZER = RECIPE_SERIALIZERS.register("basic_backpack", () -> BasicBackpackRecipe.SERIALIZER);

	public static final Supplier<MapCodec<? extends LootItemFunction>> COPY_BACKPACK_DATA = LOOT_FUNCTION_TYPES.register("copy_backpack_data", () -> CopyBackpackDataFunction.CODEC);
	public static final Supplier<MapCodec<? extends LootItemCondition>> LOOT_ENABLED_CONDITION = LOOT_CONDITION_TYPES.register("loot_enabled", () -> BackpackLootEnabledCondition.CODEC);
	public static final Supplier<MapCodec<BackpackLootModifierProvider.InjectLootModifier>> INJECT_LOOT = LOOT_MODIFIERS.register("inject_loot", () -> BackpackLootModifierProvider.InjectLootModifier.CODEC);

	private static final DeferredRegister<AttachmentType<?>> ATTACHMENT_TYPES = DeferredRegister.create(NeoForgeRegistries.Keys.ATTACHMENT_TYPES, SophisticatedBackpacks.MOD_ID);

	public static void registerHandlers(IEventBus modBus) {
		ITEMS.register(modBus);
		ModDataComponents.register(modBus);
		CREATIVE_MODE_TABS.register(modBus);
		MENU_TYPES.register(modBus);
		ENTITY_TYPES.register(modBus);
		RECIPE_SERIALIZERS.register(modBus);
		LOOT_FUNCTION_TYPES.register(modBus);
		LOOT_CONDITION_TYPES.register(modBus);
		LOOT_MODIFIERS.register(modBus);
		ATTACHMENT_TYPES.register(modBus);
		modBus.addListener(ModItems::registerContainers);
		modBus.addListener(ModItems::registerCapabilities);
		if (FMLEnvironment.getDist().isClient()) {
			ModItemsClient.init(modBus);
		}
	}

	public static final UpgradeContainerType<PickupUpgradeWrapper, ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_BASIC_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	public static final UpgradeContainerType<PickupUpgradeWrapper, ContentsFilteredUpgradeContainer<PickupUpgradeWrapper>> PICKUP_ADVANCED_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	public static final UpgradeContainerType<MagnetUpgradeWrapper, MagnetUpgradeContainer> MAGNET_BASIC_TYPE = new UpgradeContainerType<>(MagnetUpgradeContainer::new);
	public static final UpgradeContainerType<MagnetUpgradeWrapper, MagnetUpgradeContainer> MAGNET_ADVANCED_TYPE = new UpgradeContainerType<>(MagnetUpgradeContainer::new);
	public static final UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> FEEDING_TYPE = new UpgradeContainerType<>(FeedingUpgradeContainer::new);
	public static final UpgradeContainerType<FeedingUpgradeWrapper, FeedingUpgradeContainer> ADVANCED_FEEDING_TYPE = new UpgradeContainerType<>(FeedingUpgradeContainer::new);
	public static final UpgradeContainerType<CompactingUpgradeWrapper, CompactingUpgradeContainer> COMPACTING_TYPE = new UpgradeContainerType<>(CompactingUpgradeContainer::new);
	public static final UpgradeContainerType<CompactingUpgradeWrapper, CompactingUpgradeContainer> ADVANCED_COMPACTING_TYPE = new UpgradeContainerType<>(CompactingUpgradeContainer::new);
	public static final UpgradeContainerType<VoidUpgradeWrapper, VoidUpgradeContainer> VOID_TYPE = new UpgradeContainerType<>(VoidUpgradeContainer::new);
	public static final UpgradeContainerType<VoidUpgradeWrapper, VoidUpgradeContainer> ADVANCED_VOID_TYPE = new UpgradeContainerType<>(VoidUpgradeContainer::new);
	public static final UpgradeContainerType<RestockUpgradeWrapper, ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> RESTOCK_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	public static final UpgradeContainerType<RestockUpgradeWrapper, ContentsFilteredUpgradeContainer<RestockUpgradeWrapper>> ADVANCED_RESTOCK_TYPE = new UpgradeContainerType<>(ContentsFilteredUpgradeContainer::new);
	public static final UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> DEPOSIT_TYPE = new UpgradeContainerType<>(DepositUpgradeContainer::new);
	public static final UpgradeContainerType<DepositUpgradeWrapper, DepositUpgradeContainer> ADVANCED_DEPOSIT_TYPE = new UpgradeContainerType<>(DepositUpgradeContainer::new);
	public static final UpgradeContainerType<RefillUpgradeWrapper, RefillUpgradeContainer> REFILL_TYPE = new UpgradeContainerType<>(RefillUpgradeContainer::new);
	public static final UpgradeContainerType<RefillUpgradeWrapper, RefillUpgradeContainer> ADVANCED_REFILL_TYPE = new UpgradeContainerType<>(RefillUpgradeContainer::new);
	public static final UpgradeContainerType<CookingUpgradeWrapper.SmeltingUpgradeWrapper, CookingUpgradeContainer<SmeltingRecipe, CookingUpgradeWrapper.SmeltingUpgradeWrapper>> SMELTING_TYPE = new UpgradeContainerType<>(CookingUpgradeContainer::new);
	public static final UpgradeContainerType<AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper, AutoCookingUpgradeContainer<SmeltingRecipe, AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper>> AUTO_SMELTING_TYPE = new UpgradeContainerType<>(AutoCookingUpgradeContainer::new);
	public static final UpgradeContainerType<CookingUpgradeWrapper.SmokingUpgradeWrapper, CookingUpgradeContainer<SmokingRecipe, CookingUpgradeWrapper.SmokingUpgradeWrapper>> SMOKING_TYPE = new UpgradeContainerType<>(CookingUpgradeContainer::new);
	public static final UpgradeContainerType<AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper, AutoCookingUpgradeContainer<SmokingRecipe, AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper>> AUTO_SMOKING_TYPE = new UpgradeContainerType<>(AutoCookingUpgradeContainer::new);
	public static final UpgradeContainerType<CookingUpgradeWrapper.BlastingUpgradeWrapper, CookingUpgradeContainer<BlastingRecipe, CookingUpgradeWrapper.BlastingUpgradeWrapper>> BLASTING_TYPE = new UpgradeContainerType<>(CookingUpgradeContainer::new);
	public static final UpgradeContainerType<AutoCookingUpgradeWrapper.AutoBlastingUpgradeWrapper, AutoCookingUpgradeContainer<BlastingRecipe, AutoCookingUpgradeWrapper.AutoBlastingUpgradeWrapper>> AUTO_BLASTING_TYPE = new UpgradeContainerType<>(AutoCookingUpgradeContainer::new);
	public static final UpgradeContainerType<CraftingUpgradeWrapper, CraftingUpgradeContainer> CRAFTING_TYPE = new UpgradeContainerType<>(CraftingUpgradeContainer::new);
	public static final UpgradeContainerType<InceptionUpgradeWrapper, InceptionUpgradeContainer> INCEPTION_TYPE = new UpgradeContainerType<>(InceptionUpgradeContainer::new);
	public static final UpgradeContainerType<StonecutterUpgradeItem.Wrapper, StonecutterUpgradeContainer> STONECUTTER_TYPE = new UpgradeContainerType<>(StonecutterUpgradeContainer::new);
	public static final UpgradeContainerType<JukeboxUpgradeWrapper, JukeboxUpgradeContainer> JUKEBOX_TYPE = new UpgradeContainerType<>(JukeboxUpgradeContainer::new);
	public static final UpgradeContainerType<JukeboxUpgradeWrapper, JukeboxUpgradeContainer> ADVANCED_JUKEBOX_TYPE = new UpgradeContainerType<>(JukeboxUpgradeContainer::new);
	public static final UpgradeContainerType<ToolSwapperUpgradeWrapper, ToolSwapperUpgradeContainer> TOOL_SWAPPER_TYPE = new UpgradeContainerType<>(ToolSwapperUpgradeContainer::new);
	public static final UpgradeContainerType<TankUpgradeWrapper, TankUpgradeContainer> TANK_TYPE = new UpgradeContainerType<>(TankUpgradeContainer::new);
	public static final UpgradeContainerType<BatteryUpgradeWrapper, BatteryUpgradeContainer> BATTERY_TYPE = new UpgradeContainerType<>(BatteryUpgradeContainer::new);
	public static final UpgradeContainerType<PumpUpgradeWrapper, PumpUpgradeContainer> PUMP_TYPE = new UpgradeContainerType<>(PumpUpgradeContainer::new);
	public static final UpgradeContainerType<PumpUpgradeWrapper, PumpUpgradeContainer> ADVANCED_PUMP_TYPE = new UpgradeContainerType<>(PumpUpgradeContainer::new);
	public static final UpgradeContainerType<XpPumpUpgradeWrapper, XpPumpUpgradeContainer> XP_PUMP_TYPE = new UpgradeContainerType<>(XpPumpUpgradeContainer::new);
	public static final UpgradeContainerType<AnvilUpgradeWrapper, AnvilUpgradeContainer> ANVIL_TYPE = new UpgradeContainerType<>(AnvilUpgradeContainer::new);
	public static final UpgradeContainerType<SmithingUpgradeWrapper, SmithingUpgradeContainer> SMITHING_TYPE = new UpgradeContainerType<>(SmithingUpgradeContainer::new);
	public static final UpgradeContainerType<AlchemyUpgradeWrapper, AlchemyUpgradeContainer> ALCHEMY_TYPE = new UpgradeContainerType<>(AlchemyUpgradeContainer::new);
	public static final UpgradeContainerType<AlchemyUpgradeWrapper, AlchemyUpgradeContainer> ADVANCED_ALCHEMY_TYPE = new UpgradeContainerType<>(AlchemyUpgradeContainer::new);

	public static void registerContainers(RegisterEvent event) {
		if (!event.getRegistryKey().equals(Registries.MENU)) {
			return;
		}

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
		UpgradeContainerRegistry.register(ADVANCED_REFILL_UPGRADE.getId(), ADVANCED_REFILL_TYPE);
		UpgradeContainerRegistry.register(SMELTING_UPGRADE.getId(), SMELTING_TYPE);
		UpgradeContainerRegistry.register(AUTO_SMELTING_UPGRADE.getId(), AUTO_SMELTING_TYPE);
		UpgradeContainerRegistry.register(SMOKING_UPGRADE.getId(), SMOKING_TYPE);
		UpgradeContainerRegistry.register(AUTO_SMOKING_UPGRADE.getId(), AUTO_SMOKING_TYPE);
		UpgradeContainerRegistry.register(BLASTING_UPGRADE.getId(), BLASTING_TYPE);
		UpgradeContainerRegistry.register(AUTO_BLASTING_UPGRADE.getId(), AUTO_BLASTING_TYPE);
		UpgradeContainerRegistry.register(CRAFTING_UPGRADE.getId(), CRAFTING_TYPE);
		UpgradeContainerRegistry.register(INCEPTION_UPGRADE.getId(), INCEPTION_TYPE);
		UpgradeContainerRegistry.register(STONECUTTER_UPGRADE.getId(), STONECUTTER_TYPE);
		UpgradeContainerRegistry.register(JUKEBOX_UPGRADE.getId(), JUKEBOX_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_JUKEBOX_UPGRADE.getId(), ADVANCED_JUKEBOX_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_TOOL_SWAPPER_UPGRADE.getId(), TOOL_SWAPPER_TYPE);
		UpgradeContainerRegistry.register(TANK_UPGRADE.getId(), TANK_TYPE);
		UpgradeContainerRegistry.register(BATTERY_UPGRADE.getId(), BATTERY_TYPE);
		UpgradeContainerRegistry.register(PUMP_UPGRADE.getId(), PUMP_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_PUMP_UPGRADE.getId(), ADVANCED_PUMP_TYPE);
		UpgradeContainerRegistry.register(XP_PUMP_UPGRADE.getId(), XP_PUMP_TYPE);
		UpgradeContainerRegistry.register(ANVIL_UPGRADE.getId(), ANVIL_TYPE);
		UpgradeContainerRegistry.register(SMITHING_UPGRADE.getId(), SMITHING_TYPE);
		UpgradeContainerRegistry.register(ALCHEMY_UPGRADE.getId(), ALCHEMY_TYPE);
		UpgradeContainerRegistry.register(ADVANCED_ALCHEMY_UPGRADE.getId(), ADVANCED_ALCHEMY_TYPE);
	}

	public static void registerDispenseBehavior() {
		DispenserBlock.registerBehavior(BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(COPPER_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(IRON_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(GOLD_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(DIAMOND_BACKPACK.get(), new BackpackDispenseBehavior());
		DispenserBlock.registerBehavior(NETHERITE_BACKPACK.get(), new BackpackDispenseBehavior());
	}

	public static void registerCauldronInteractions() {
		BackpackCauldronInteraction interaction = new BackpackCauldronInteraction();
		CauldronInteractions.WATER.put(BACKPACK.get(), interaction);
		CauldronInteractions.WATER.put(COPPER_BACKPACK.get(), interaction);
		CauldronInteractions.WATER.put(IRON_BACKPACK.get(), interaction);
		CauldronInteractions.WATER.put(GOLD_BACKPACK.get(), interaction);
		CauldronInteractions.WATER.put(DIAMOND_BACKPACK.get(), interaction);
		CauldronInteractions.WATER.put(NETHERITE_BACKPACK.get(), interaction);
	}

	private static void registerCapabilities(RegisterCapabilitiesEvent event) {
		event.registerItem(Capabilities.Item.ITEM, (stack, itemAccess) -> {
					IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
					return backpackWrapper.getContentsUuid().isEmpty() ? EmptyResourceHandler.instance() : backpackWrapper.getInventoryForInputOutput();
				},
				BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
		event.registerItem(Capabilities.Fluid.ITEM, (stack, itemAccess) -> {
					if (!Config.SERVER.itemFluidHandlerEnabled.get()) {
						return null;
					}
					return BackpackWrapper.fromStack(stack).getItemFluidHandler(itemAccess).orElse(null);
				},
				BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
		event.registerItem(Capabilities.Energy.ITEM, (stack, v) -> BackpackWrapper.fromStack(stack).getEnergyHandler().orElse(null),
				BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}

	private static class BackpackCauldronInteraction implements CauldronInteraction {
		private static boolean hasDefaultColor(IStorageWrapper wrapper) {
			return wrapper.getAccentColor() == BackpackWrapper.DEFAULT_ACCENT_COLOR && wrapper.getMainColor() == BackpackWrapper.DEFAULT_MAIN_COLOR;
		}

		@Override
		public InteractionResult interact(BlockState state, Level level, BlockPos pos, Player player, InteractionHand hand, ItemStack stack) {
			IBackpackWrapper backpackWrapper = BackpackWrapper.fromStack(stack);
			if (hasDefaultColor(backpackWrapper)) {
				return InteractionResult.FAIL;
			}

			if (!level.isClientSide()) {
				backpackWrapper.setColors(BackpackWrapper.DEFAULT_MAIN_COLOR, BackpackWrapper.DEFAULT_ACCENT_COLOR);
				LayeredCauldronBlock.lowerFillLevel(state, level, pos);
			}

			return InteractionResult.SUCCESS;
		}
	}

	private static class BackpackDispenseBehavior extends OptionalDispenseItemBehavior {
		@Override
		protected ItemStack execute(BlockSource source, ItemStack stack) {
			setSuccess(false);
			Item item = stack.getItem();
			if (item instanceof BackpackItem backpackItem) {
				Direction dispenserDirection = source.state().getValue(DispenserBlock.FACING);
				BlockPos blockpos = source.pos().relative(dispenserDirection);
				Direction against = source.level().isEmptyBlock(blockpos.below()) ? dispenserDirection.getOpposite() : Direction.UP;

				setSuccess(backpackItem.tryPlace(null, dispenserDirection.getAxis() == Direction.Axis.Y ? Direction.NORTH : dispenserDirection.getOpposite(), new DirectionalPlaceContext(source.level(), blockpos, dispenserDirection, stack, against)).consumesAction());
			}

			return stack;
		}
	}
}
