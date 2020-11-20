package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.block.Blocks;
import net.minecraft.data.CustomRecipeBuilder;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.IFinishedRecipe;
import net.minecraft.data.ShapedRecipeBuilder;
import net.minecraftforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackSingleDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackTwoDyesRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.ShapeBasedRecipeBuilder;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.UpgradeNextTierRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper.getModRegistryName;

public class Recipes extends net.minecraft.data.RecipeProvider {
	public Recipes(DataGenerator generatorIn) {
		super(generatorIn);
	}

	@Override
	protected void registerRecipes(Consumer<IFinishedRecipe> consumer) {
		ShapedRecipeBuilder.shapedRecipe(ModItems.BACKPACK.get())
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
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.GOLD_BACKPACK.get(), BackpackUpgradeRecipe.SERIALIZER)
				.patternLine("GGG")
				.patternLine("GBG")
				.patternLine("GGG")
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('B', ModItems.IRON_BACKPACK.get())
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.IRON_BACKPACK.get(), BackpackUpgradeRecipe.SERIALIZER)
				.patternLine("III")
				.patternLine("IBI")
				.patternLine("III")
				.key('I', Tags.Items.INGOTS_IRON)
				.key('B', ModItems.BACKPACK.get())
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.PICKUP_UPGRADE.get())
				.patternLine(" P ")
				.patternLine("SBS")
				.patternLine("RRR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('S', Tags.Items.STRING)
				.key('P', Blocks.STICKY_PISTON)
				.addCriterion("has_upgrade_base", hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.UPGRADE_BASE.get())
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
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.FILTER_UPGRADE.get())
				.patternLine("RSR")
				.patternLine("SBS")
				.patternLine("RSR")
				.key('B', ModItems.UPGRADE_BASE.get())
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('S', Tags.Items.STRING)
				.addCriterion("has_upgrade_base", hasItem(ModItems.UPGRADE_BASE.get()))
				.build(consumer);

		ShapeBasedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_FILTER_UPGRADE.get(), UpgradeNextTierRecipe.SERIALIZER)
				.patternLine("GPG")
				.patternLine("RRR")
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', ModItems.FILTER_UPGRADE.get())
				.build(consumer);
	}
}
