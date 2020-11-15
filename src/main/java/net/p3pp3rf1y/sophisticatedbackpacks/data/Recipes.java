package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.block.Blocks;
import net.minecraft.data.CustomRecipeBuilder;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.IFinishedRecipe;
import net.minecraft.data.ShapedRecipeBuilder;
import net.minecraftforge.common.Tags;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackSingleDyeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackTwoDyesRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipeBuilder;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper.getModRegistryName;

public class Recipes extends net.minecraft.data.RecipeProvider {
	public Recipes(DataGenerator generatorIn) {
		super(generatorIn);
	}

	@Override
	protected void registerRecipes(Consumer<IFinishedRecipe> consumer) {
		ShapedRecipeBuilder.shapedRecipe(ModItems.BACKPACK)
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

		BackpackUpgradeRecipeBuilder.shapedRecipe(ModItems.DIAMOND_BACKPACK)
				.patternLine("DDD")
				.patternLine("DBD")
				.patternLine("DDD")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('B', ModItems.GOLD_BACKPACK)
				.build(consumer);

		BackpackUpgradeRecipeBuilder.shapedRecipe(ModItems.GOLD_BACKPACK)
				.patternLine("GGG")
				.patternLine("GBG")
				.patternLine("GGG")
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('B', ModItems.IRON_BACKPACK)
				.build(consumer);

		BackpackUpgradeRecipeBuilder.shapedRecipe(ModItems.IRON_BACKPACK)
				.patternLine("III")
				.patternLine("IBI")
				.patternLine("III")
				.key('I', Tags.Items.INGOTS_IRON)
				.key('B', ModItems.BACKPACK)
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.PICKUP_UPGRADE)
				.patternLine(" P ")
				.patternLine("SBS")
				.patternLine("RRR")
				.key('B', ModItems.UPGRADE_BASE)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('S', Tags.Items.STRING)
				.key('P', Blocks.STICKY_PISTON)
				.addCriterion("has_upgrade_base", hasItem(ModItems.UPGRADE_BASE))
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.UPGRADE_BASE)
				.patternLine("SIS")
				.patternLine("ILI")
				.patternLine("SIS")
				.key('L', Tags.Items.LEATHER)
				.key('I', Tags.Items.INGOTS_IRON)
				.key('S', Tags.Items.STRING)
				.addCriterion("has_leather", hasItem(Tags.Items.LEATHER))
				.build(consumer);

		ShapedRecipeBuilder.shapedRecipe(ModItems.ADVANCED_PICKUP_UPGRADE)
				.patternLine(" D ")
				.patternLine("GPG")
				.patternLine("RRR")
				.key('D', Tags.Items.GEMS_DIAMOND)
				.key('G', Tags.Items.INGOTS_GOLD)
				.key('R', Tags.Items.DUSTS_REDSTONE)
				.key('P', ModItems.PICKUP_UPGRADE)
				.addCriterion("has_pickup_upgrade", hasItem(ModItems.PICKUP_UPGRADE))
				.build(consumer);
	}
}
