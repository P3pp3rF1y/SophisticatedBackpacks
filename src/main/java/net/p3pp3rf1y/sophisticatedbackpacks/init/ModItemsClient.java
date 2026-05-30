package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.world.item.crafting.BlastingRecipe;
import net.minecraft.world.item.crafting.SmeltingRecipe;
import net.minecraft.world.item.crafting.SmokingRecipe;
import net.neoforged.bus.api.IEventBus;
import net.neoforged.neoforge.client.event.RegisterMenuScreensEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil.AnvilUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherInventoryControl;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher.MobCatcherUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing.SmithingUpgradeTab;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeGuiManager;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.compat.trashslot.TrashSlotScreenRegistry;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ContentsFilteredUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.alchemy.AlchemyUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryInventoryControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.battery.BatteryUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.compacting.CompactingUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.AutoCookingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.AutoCookingUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.AutoCookingUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.CookingUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.FeedingUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.feeding.FeedingUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.filter.FilterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.filter.FilterUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.magnet.MagnetUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pickup.PickupUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pickup.PickupUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.pump.PumpUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter.StonecutterUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter.StonecutterUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankInventoryControl;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.voiding.VoidUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.xppump.XpPumpUpgradeTab;

public class ModItemsClient {
	public static void init(IEventBus modBus) {
		modBus.addListener(ModItemsClient::onMenuScreenRegister);
	}

	private static void onMenuScreenRegister(RegisterMenuScreensEvent event) {
		event.register(ModItems.BACKPACK_CONTAINER_TYPE.get(), BackpackScreen::constructScreen);
		event.register(ModItems.SETTINGS_CONTAINER_TYPE.get(), BackpackSettingsScreen::constructScreen);

		UpgradeGuiManager.registerTab(ModItems.PICKUP_BASIC_TYPE, (ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new PickupUpgradeTab.Basic(uc, p, s, Config.SERVER.pickupUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.PICKUP_ADVANCED_TYPE, (ContentsFilteredUpgradeContainer<PickupUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new PickupUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedPickupUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(FilterUpgradeContainer.BASIC_TYPE, (FilterUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new FilterUpgradeTab.Basic(uc, p, s, Config.SERVER.filterUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(FilterUpgradeContainer.ADVANCED_TYPE, (FilterUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new FilterUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedFilterUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.MAGNET_BASIC_TYPE, (MagnetUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new MagnetUpgradeTab.Basic(uc, p, s, Config.SERVER.magnetUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.MAGNET_ADVANCED_TYPE, (MagnetUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new MagnetUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedMagnetUpgrade.slotsInRow.get(), SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.FEEDING_TYPE, (FeedingUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new FeedingUpgradeTab.Basic(uc, p, s, Config.SERVER.feedingUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_FEEDING_TYPE, (FeedingUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new FeedingUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedFeedingUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.COMPACTING_TYPE, (CompactingUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new CompactingUpgradeTab.Basic(uc, p, s, Config.SERVER.compactingUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_COMPACTING_TYPE, (CompactingUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new CompactingUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedCompactingUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.VOID_TYPE, (VoidUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new VoidUpgradeTab.Basic(uc, p, s, Config.SERVER.voidUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_VOID_TYPE, (VoidUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new VoidUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedVoidUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.RESTOCK_TYPE, (ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new RestockUpgradeTab.Basic(uc, p, s, SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_RESTOCK_TYPE, (ContentsFilteredUpgradeContainer<RestockUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new RestockUpgradeTab.Advanced(uc, p, s, SBPButtonDefinitions.BACKPACK_CONTENTS_FILTER_TYPE));
		UpgradeGuiManager.registerTab(ModItems.DEPOSIT_TYPE, DepositUpgradeTab.Basic::new);
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_DEPOSIT_TYPE, DepositUpgradeTab.Advanced::new);
		UpgradeGuiManager.registerTab(ModItems.REFILL_TYPE, (RefillUpgradeContainer uc, Position p, StorageScreenBase<?> s) -> new RefillUpgradeTab.Basic(uc, p, s,
				Config.SERVER.refillUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_REFILL_TYPE, (RefillUpgradeContainer uc, Position p, StorageScreenBase<?> s) -> new RefillUpgradeTab.Advanced(uc, p, s,
				Config.SERVER.advancedRefillUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.SMELTING_TYPE, CookingUpgradeTab.SmeltingUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.AUTO_SMELTING_TYPE, (AutoCookingUpgradeContainer<SmeltingRecipe, AutoCookingUpgradeWrapper.AutoSmeltingUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new AutoCookingUpgradeTab.AutoSmeltingUpgradeTab(uc, p, s, Config.SERVER.autoSmeltingUpgrade.inputFilterSlotsInRow.get(), Config.SERVER.autoSmeltingUpgrade.fuelFilterSlotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.SMOKING_TYPE, CookingUpgradeTab.SmokingUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.AUTO_SMOKING_TYPE, (AutoCookingUpgradeContainer<SmokingRecipe, AutoCookingUpgradeWrapper.AutoSmokingUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new AutoCookingUpgradeTab.AutoSmokingUpgradeTab(uc, p, s, Config.SERVER.autoSmokingUpgrade.inputFilterSlotsInRow.get(), Config.SERVER.autoSmokingUpgrade.fuelFilterSlotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.BLASTING_TYPE, CookingUpgradeTab.BlastingUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.AUTO_BLASTING_TYPE, (AutoCookingUpgradeContainer<BlastingRecipe, AutoCookingUpgradeWrapper.AutoBlastingUpgradeWrapper> uc, Position p, StorageScreenBase<?> s) ->
				new AutoCookingUpgradeTab.AutoBlastingUpgradeTab(uc, p, s, Config.SERVER.autoBlastingUpgrade.inputFilterSlotsInRow.get(), Config.SERVER.autoBlastingUpgrade.fuelFilterSlotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.CRAFTING_TYPE, (CraftingUpgradeContainer uc, Position p, StorageScreenBase<?> s) ->
				new CraftingUpgradeTab(uc, p, s, SBPButtonDefinitions.SHIFT_CLICK_TARGET, SBPButtonDefinitions.REFILL_CRAFTING_GRID));
		UpgradeGuiManager.registerTab(ModItems.INCEPTION_TYPE, InceptionUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.STONECUTTER_TYPE, (StonecutterUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) ->
				new StonecutterUpgradeTab(upgradeContainer, position, screen, SBPButtonDefinitions.SHIFT_CLICK_TARGET, SBPButtonDefinitions.REFILL_INPUT));
		UpgradeGuiManager.registerTab(ModItems.JUKEBOX_TYPE, JukeboxUpgradeTab.Basic::new);
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_JUKEBOX_TYPE, (JukeboxUpgradeContainer uc, Position p, StorageScreenBase<?> s) -> new JukeboxUpgradeTab.Advanced(uc, p, s, Config.SERVER.advancedJukeboxUpgrade.slotsInRow.get()));
		UpgradeGuiManager.registerTab(ModItems.TOOL_SWAPPER_TYPE, ToolSwapperUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.TANK_TYPE, TankUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.BATTERY_TYPE, BatteryUpgradeTab::new);
		UpgradeGuiManager.registerInventoryControl(ModItems.TANK_TYPE, TankInventoryControl::new);
		UpgradeGuiManager.registerInventoryControl(ModItems.BATTERY_TYPE, BatteryInventoryControl::new);
		UpgradeGuiManager.registerInventoryControl(MobCatcherUpgradeItem.TYPE, MobCatcherInventoryControl::create);
		UpgradeGuiManager.registerTab(ModItems.PUMP_TYPE, PumpUpgradeTab.Basic::new);
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_PUMP_TYPE, PumpUpgradeTab.Advanced::new);
		UpgradeGuiManager.registerTab(ModItems.XP_PUMP_TYPE, (XpPumpUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) ->
				new XpPumpUpgradeTab(upgradeContainer, position, screen, Config.SERVER.xpPumpUpgrade.mendingOn.get()));
		UpgradeGuiManager.registerTab(ModItems.ANVIL_TYPE, AnvilUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.SMITHING_TYPE, SmithingUpgradeTab::new);
		UpgradeGuiManager.registerTab(ModItems.ALCHEMY_TYPE, AlchemyUpgradeTab.Basic::new);
		UpgradeGuiManager.registerTab(ModItems.ADVANCED_ALCHEMY_TYPE, AlchemyUpgradeTab.Advanced::new);

		TrashSlotScreenRegistry.registerScreen(BackpackScreen.class);
	}
}
