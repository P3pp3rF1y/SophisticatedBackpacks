package net.p3pp3rf1y.sophisticatedbackpacks.compat.chipped;

import earth.terrarium.chipped.common.compat.jei.ChippedRecipeCategory;
import earth.terrarium.chipped.common.registry.ModRecipeTypes;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.DistExecutor;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.RegisterEvent;
import net.minecraftforge.registries.RegistryObject;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPButtonDefinitions;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.jei.SBPPlugin;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeGuiManager;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerRegistry;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;
import net.p3pp3rf1y.sophisticatedcore.compat.chipped.BlockTransformationUpgradeContainer;
import net.p3pp3rf1y.sophisticatedcore.compat.chipped.BlockTransformationUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.compat.chipped.BlockTransformationUpgradeTab;
import net.p3pp3rf1y.sophisticatedcore.compat.chipped.BlockTransformationUpgradeWrapper;

import java.util.function.Supplier;

public class ChippedCompat implements ICompat {

	public static final RegistryObject<BlockTransformationUpgradeItem> BOTANIST_WORKBENCH_UPGRADE = ModItems.ITEMS.register("chipped/botanist_workbench_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.BOTANIST_WORKBENCH, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> GLASSBLOWER_UPGRADE = ModItems.ITEMS.register("chipped/glassblower_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.GLASSBLOWER, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> CARPENTERS_TABLE_UPGRADE = ModItems.ITEMS.register("chipped/carpenters_table_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.CARPENTERS_TABLE, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> LOOM_TABLE_UPGRADE = ModItems.ITEMS.register("chipped/loom_table_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.LOOM_TABLE, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> MASON_TABLE_UPGRADE = ModItems.ITEMS.register("chipped/mason_table_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.MASON_TABLE, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> ALCHEMY_BENCH_UPGRADE = ModItems.ITEMS.register("chipped/alchemy_bench_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.ALCHEMY_BENCH, Config.SERVER.maxUpgradesPerStorage));
	public static final RegistryObject<BlockTransformationUpgradeItem> TINKERING_TABLE_UPGRADE = ModItems.ITEMS.register("chipped/tinkering_table_upgrade",
			() -> new BlockTransformationUpgradeItem(ModRecipeTypes.TINKERING_TABLE, Config.SERVER.maxUpgradesPerStorage));

	@Override
	public void init() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		modBus.addListener(this::registerContainers);

		if (ModList.get().isLoaded(CompatModIds.JEI)) {
			((Supplier<Runnable>) () -> () -> SBPPlugin.setAdditionalCatalystRegistrar(registration -> {
				registration.addRecipeCatalyst(new ItemStack(BOTANIST_WORKBENCH_UPGRADE.get()), ChippedRecipeCategory.BOTANIST_WORKBENCH_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(GLASSBLOWER_UPGRADE.get()), ChippedRecipeCategory.GLASSBLOWER_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(CARPENTERS_TABLE_UPGRADE.get()), ChippedRecipeCategory.CARPENTERS_TABLE_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(LOOM_TABLE_UPGRADE.get()), ChippedRecipeCategory.LOOM_TABLE_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(MASON_TABLE_UPGRADE.get()), ChippedRecipeCategory.MASON_TABLE_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(ALCHEMY_BENCH_UPGRADE.get()), ChippedRecipeCategory.ALCHEMY_BENCH_RECIPE);
				registration.addRecipeCatalyst(new ItemStack(TINKERING_TABLE_UPGRADE.get()), ChippedRecipeCategory.TINKERING_TABLE_RECIPE);
			})).get().run();
		}
	}

	public void registerContainers(RegisterEvent event) {
		if (!event.getRegistryKey().equals(ForgeRegistries.Keys.MENU_TYPES)) {
			return;
		}
		registerUpgradeContainer(BOTANIST_WORKBENCH_UPGRADE);
		registerUpgradeContainer(GLASSBLOWER_UPGRADE);
		registerUpgradeContainer(CARPENTERS_TABLE_UPGRADE);
		registerUpgradeContainer(LOOM_TABLE_UPGRADE);
		registerUpgradeContainer(MASON_TABLE_UPGRADE);
		registerUpgradeContainer(ALCHEMY_BENCH_UPGRADE);
		registerUpgradeContainer(TINKERING_TABLE_UPGRADE);
	}

	private void registerUpgradeContainer(RegistryObject<BlockTransformationUpgradeItem> item) {
		UpgradeContainerType<BlockTransformationUpgradeWrapper, BlockTransformationUpgradeContainer> containerType = new UpgradeContainerType<>(BlockTransformationUpgradeContainer::new);
		UpgradeContainerRegistry.register(item.getId(), containerType);
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> UpgradeGuiManager.registerTab(containerType, (BlockTransformationUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) -> {
			String itemName = item.getId().getPath();
			return new BlockTransformationUpgradeTab(upgradeContainer, position, screen, SBPButtonDefinitions.SHIFT_CLICK_TARGET, itemName.replace('/', '_').substring(0, itemName.length() - "_upgrade".length()));
		}));
	}

	@Override
	public void setup() {
		//noop
	}
}
