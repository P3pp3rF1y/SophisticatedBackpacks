package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.RecipeTypes;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandlerHelper;
import mezz.jei.api.registration.*;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.item.crafting.SmithingRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.DyeRecipesMaker;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.CraftingContainerRecipeTransferHandlerBase;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.SettingsGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.StorageGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.subtypes.JeiSubtypeInterpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.SubtypeInterpreters.getSubtypeInterpreters;

@SuppressWarnings("unused")
@JeiPlugin
public class SBPPlugin implements IModPlugin {
	private static Consumer<IRecipeCatalystRegistration> additionalCatalystRegistrar = registration -> {};
	public static void setAdditionalCatalystRegistrar(Consumer<IRecipeCatalystRegistration> additionalCatalystRegistrar) {
		SBPPlugin.additionalCatalystRegistrar = additionalCatalystRegistrar;
	}

	@Override
	public ResourceLocation getPluginUid() {
		return ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "default");
	}

	@Override
	public void registerItemSubtypes(ISubtypeRegistration registration) {
		getSubtypeInterpreters()
				.forEach((item, subtypeInterpreter) -> registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, item, JeiSubtypeInterpreter.of(subtypeInterpreter)));
	}

	@Override
	public void registerGuiHandlers(IGuiHandlerRegistration registration) {
		registration.addGuiContainerHandler(BackpackScreen.class, new IGuiContainerHandler<>() {
			@Override
			public List<Rect2i> getGuiExtraAreas(BackpackScreen gui) {
				//noinspection ConstantValue
				if (gui.getUpgradeSettingsControl() == null) {
					return new ArrayList<>(); // when CarryOn cancels opening of the screen it doesn't seem to cancel the even going to JEI and then control is null here
				}

				List<Rect2i> ret = new ArrayList<>();
				gui.getUpgradeSlotsRectangle().ifPresent(ret::add);
				ret.addAll(gui.getUpgradeSettingsControl().getTabRectangles());
				gui.getSortButtonsRectangle().ifPresent(ret::add);
				return ret;
			}
		});

		registration.addGuiContainerHandler(BackpackSettingsScreen.class, new IGuiContainerHandler<>() {
			@Override
			public List<Rect2i> getGuiExtraAreas(BackpackSettingsScreen gui) {
				return new ArrayList<>(gui.getSettingsTabControl().getTabRectangles());
			}
		});

		registration.addGhostIngredientHandler(BackpackScreen.class, new StorageGhostIngredientHandler<>());
		registration.addGhostIngredientHandler(SettingsScreen.class, new SettingsGhostIngredientHandler<>());
	}

	@Override
	public void registerRecipes(IRecipeRegistration registration) {
		registration.addRecipes(RecipeTypes.CRAFTING, DyeRecipesMaker.getRecipes());
		registration.addRecipes(RecipeTypes.CRAFTING, ClientRecipeHelper.transformAllRecipesOfType(RecipeType.CRAFTING, BackpackUpgradeRecipe.class, JeiClientRecipeHelper::copyShapedRecipeWithRecipeHolder));
	}

	@Override
	public void registerRecipeCatalysts(IRecipeCatalystRegistration registration) {
		registration.addRecipeCatalyst(new ItemStack(ModItems.CRAFTING_UPGRADE.get()), RecipeTypes.CRAFTING);
		registration.addRecipeCatalyst(new ItemStack(ModItems.STONECUTTER_UPGRADE.get()), RecipeTypes.STONECUTTING);
		registration.addRecipeCatalyst(new ItemStack(ModItems.SMITHING_UPGRADE.get()), RecipeTypes.SMITHING);
		additionalCatalystRegistrar.accept(registration);
	}

	@Override
	public void registerRecipeTransferHandlers(IRecipeTransferRegistration registration) {
		IRecipeTransferHandlerHelper handlerHelper = registration.getTransferHelper();
		IStackHelper stackHelper = registration.getJeiHelpers().getStackHelper();
		registration.addRecipeTransferHandler(new CraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<CraftingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public mezz.jei.api.recipe.RecipeType<RecipeHolder<CraftingRecipe>> getRecipeType() {
				return RecipeTypes.CRAFTING;
			}
		}, RecipeTypes.CRAFTING);

		registration.addRecipeTransferHandler(new CraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<SmithingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public mezz.jei.api.recipe.RecipeType<RecipeHolder<SmithingRecipe>> getRecipeType() {
				return RecipeTypes.SMITHING;
			}
		}, RecipeTypes.SMITHING);
	}
}
