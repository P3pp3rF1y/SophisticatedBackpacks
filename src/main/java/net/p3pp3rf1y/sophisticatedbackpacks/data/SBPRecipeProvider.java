package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.block.Blocks;
import net.minecraft.data.CustomRecipeBuilder;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.IFinishedRecipe;
import net.minecraft.data.RecipeProvider;
import net.minecraft.item.Items;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackSingleDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackTwoDyesRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.ShapeBasedRecipeBuilder;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper.getModRegistryName;

public class SBPRecipeProvider extends RecipeProvider {

	private static final String HAS_UPGRADE_BASE_CRITERION = "has_upgrade_base";

	public SBPRecipeProvider(DataGenerator generatorIn) {
		super(generatorIn);
	}

	@Override
	protected void registerRecipes(Consumer<IFinishedRecipe> consumer) {
		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.BACKPACK.get())
				.patternLine("SLS")
				.patternLine("SCS")
				.patternLine("LLL")
				.key('L', Tags.Items.LEATHER)
				.key('C', Tags.Items.CHESTS_WOODEN)
				.key('S', Tags.Items.STRING)
				.addCriterion("has_leather", hasItem(Tags.Items.LEATHER))
				.build(consumer);

		CustomRecipeBuilder.customRecipe(BackpackSingleDyeRecipe.SERIALIZER).build(consumer, getModRegistryName("backpack_single_dye"));
		CustomRecipeBuilder.customRecipe(BackpackTwoDyesRecipe.SERIALIZER).build(consumer, getModRegistryName("backpack_two_dyes"));

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.DIAMOND_BACKPACK.get(), BackpackUpgradeRecipe.SERIALIZER)
				.patternLine("DDD")
				.patternLine("DBD")
				.patternLine("DDD")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('B', ModItems.GOLD_BACKPACK.get())
				.addCriterion("has_gold_backpack", hasItem(ModItems.GOLD_BACKPACK.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.GOLD_BACKPACK.get(), BackpackUpgradeRecipe.SERIALIZER)
				.patternLine("GGG")
				.patternLine("GBG")
				.patternLine("GGG")
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('B', ModItems.IRON_BACKPACK.get())
				.addCriterion("has_iron_backpack", hasItem(ModItems.IRON_BACKPACK.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.IRON_BACKPACK.get(), BackpackUpgradeRecipe.SERIALIZER)
				.patternLine("III")
				.patternLine("IBI")
				.patternLine("III")
				.key('I', Tags.Items.INGOTS_IRON)
				.key('B', ModItems.BACKPACK.get())
				.addCriterion("has_backpack", hasItem(ModItems.BACKPACK.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.PICKUP_UPGRADE.get())
				.patternLine(" P ")
				.patternLine("SBS")
				.patternLine("RRR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('S', Tags.Items.STRING)
				.key('P', Blocks.STICKY_PISTON)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.UPGRADE_BASE.get())
				.patternLine("SIS")
				.patternLine("ILI")
				.patternLine("SIS")
				.key('L', Tags.Items.LEATHER)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('S', Tags.Items.STRING)
				.addCriterion("has_leather", hasItem(Tags.Items.LEATHER))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_PICKUP_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GPG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', ModItems.PICKUP_UPGRADE.get())
				.addCriterion("has_pickup_upgrade", hasItem(ModItems.PICKUP_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.FILTER_UPGRADE.get())
				.patternLine("RSR")
				.patternLine("SBS")
				.patternLine("RSR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('S', Tags.Items.STRING)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_FILTER_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine("GPG")
				.patternLine("RRR")
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', ModItems.FILTER_UPGRADE.get())
				.addCriterion("has_filter_upgrade", hasItem(ModItems.FILTER_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.MAGNET_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine("EIE")
				.patternLine("IPI")
				.patternLine("R L")
				.key('E', Tags.Items.ENDER_PEARLS)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('L', Tags.Items.GEMS_LAPIS)
				.key('P', ModItems.PICKUP_UPGRADE.get())
				.addCriterion("has_pickup_upgrade", hasItem(ModItems.PICKUP_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_MAGNET_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine("EIE")
				.patternLine("IPI")
				.patternLine("R L")
				.key('E', Tags.Items.ENDER_PEARLS)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('L', Tags.Items.GEMS_LAPIS)
				.key('P', ModItems.ADVANCED_PICKUP_UPGRADE.get())
				.addCriterion("has_advanced_pickup_upgrade", hasItem(ModItems.ADVANCED_PICKUP_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_MAGNET_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GMG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('M', ModItems.MAGNET_UPGRADE.get())
				.addCriterion("has_magnet_upgrade", hasItem(ModItems.MAGNET_UPGRADE.get()))
				.build(consumer, new ResourceLocation(RegistryHelper.getModRegistryName("advanced_magnet_upgrade_from_basic")));

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.FEEDING_UPGRADE.get())
				.patternLine(" C ")
				.patternLine("ABM")
				.patternLine(" E ")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('C', Items.GOLDEN_CARROT)
				.key('A', Items.GOLDEN_APPLE)
				.key('M', Items.GLISTERING_MELON_SLICE)
				.key('E', Tags.Items.ENDER_PEARLS)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.COMPACTING_UPGRADE.get())
				.patternLine("IPI")
				.patternLine("PBP")
				.patternLine("RPR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('I', Tags.Items.INGOTS_IRON)
				.key('P', Items.PISTON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_COMPACTING_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GCG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('C', ModItems.COMPACTING_UPGRADE.get())
				.addCriterion("has_compacting_upgrade", hasItem(ModItems.COMPACTING_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.VOID_UPGRADE.get())
				.patternLine(" E ")
				.patternLine("OBO")
				.patternLine("ROR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('E', Tags.Items.ENDER_PEARLS)
				.key('O', Tags.Items.OBSIDIAN)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_VOID_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GVG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('V', ModItems.VOID_UPGRADE.get())
				.addCriterion("has_void_upgrade", hasItem(ModItems.VOID_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.RESTOCK_UPGRADE.get())
				.patternLine(" P ")
				.patternLine("IBI")
				.patternLine("RCR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('C', Tags.Items.CHESTS_WOODEN)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', Items.STICKY_PISTON)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_RESTOCK_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GVG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('V', ModItems.RESTOCK_UPGRADE.get())
				.addCriterion("has_restock_upgrade", hasItem(ModItems.RESTOCK_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.DEPOSIT_UPGRADE.get())
				.patternLine(" P ")
				.patternLine("IBI")
				.patternLine("RCR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('C', Tags.Items.CHESTS_WOODEN)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', Items.PISTON)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_DEPOSIT_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine(" D ")
				.patternLine("GVG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('V', ModItems.DEPOSIT_UPGRADE.get())
				.addCriterion("has_deposit_upgrade", hasItem(ModItems.DEPOSIT_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.REFILL_UPGRADE.get())
				.patternLine(" E ")
				.patternLine("IBI")
				.patternLine("RCR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('C', Tags.Items.CHESTS_WOODEN)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('E', Tags.Items.ENDER_PEARLS)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.INCEPTION_UPGRADE.get())
				.patternLine("ESE")
				.patternLine("DBD")
				.patternLine("EDE")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('S', Tags.Items.NETHER_STARS)
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('E', Items.ENDER_EYE)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.EVERLASTING_UPGRADE.get())
				.patternLine("CSC")
				.patternLine("SBS")
				.patternLine("CSC")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('S', Tags.Items.NETHER_STARS)
				.key('C', Items.END_CRYSTAL)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.SMELTING_UPGRADE.get())
				.patternLine("RIR")
				.patternLine("IBI")
				.patternLine("RFR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('F', Items.FURNACE)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.AUTO_SMELTING_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine("DHD")
				.patternLine("RSH")
				.patternLine("GHG")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('H', Items.HOPPER)
				.key('S', ModItems.SMELTING_UPGRADE.get())
				.addCriterion("has_smelting_upgrade", hasItem(ModItems.SMELTING_UPGRADE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.CRAFTING_UPGRADE.get())
				.patternLine(" T ")
				.patternLine("IBI")
				.patternLine(" C ")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('C', Tags.Items.CHESTS)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('T', Items.CRAFTING_TABLE)
				.addCriterion(HAS_UPGRADE_BASE_CRITERION, hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);
	}
}
