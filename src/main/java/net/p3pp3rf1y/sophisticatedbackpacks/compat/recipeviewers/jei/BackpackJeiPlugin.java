package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.RecipeTypes;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandlerHelper;
import mezz.jei.api.recipe.types.IRecipeType;
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
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.ClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiCraftingContainerRecipeTransferHandlerBase;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiRecipeDisplayGenerator;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiSettingsGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiStorageGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.subtypes.JeiSubtypeInterpreter;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.SubtypeInterpreters.getSubtypeInterpreters;

@SuppressWarnings("unused")
@JeiPlugin
public class BackpackJeiPlugin implements IModPlugin {
	private static Consumer<IRecipeCatalystRegistration> additionalCatalystRegistrar = registration -> {};
	public static void addAdditionalCatalystRegistrar(Consumer<IRecipeCatalystRegistration> additionalCatalystRegistrar) {
		BackpackJeiPlugin.additionalCatalystRegistrar = BackpackJeiPlugin.additionalCatalystRegistrar.andThen(additionalCatalystRegistrar);
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
				return new ArrayList<>(gui.getExtendedControlsRectangles());
			}
		});

		registration.addGhostIngredientHandler(BackpackScreen.class, new JeiStorageGhostIngredientHandler<>());
		registration.addGhostIngredientHandler(SettingsScreen.class, new JeiSettingsGhostIngredientHandler<>());
	}

	@Override
	public void registerRecipes(IRecipeRegistration registration) {
		JeiRecipeDisplayGenerator generator = new JeiRecipeDisplayGenerator();

		DyeRecipesMaker.addRecipes(generator);
		ClientRecipeHelper.addAllRecipesOfType(generator, RecipeType.CRAFTING, BackpackUpgradeRecipe.class);
		ClientRecipeHelper.addAllRecipesOfType(generator, RecipeType.SMITHING, SmithingBackpackUpgradeRecipe.class);

		registration.addRecipes(RecipeTypes.CRAFTING, generator.getCraftingRecipes());
		registration.addRecipes(RecipeTypes.SMITHING, generator.getSmithingRecipes());
	}

	@Override
	public void registerRecipeCatalysts(IRecipeCatalystRegistration registration) {
		registration.addCraftingStation(RecipeTypes.CRAFTING, new ItemStack(ModItems.CRAFTING_UPGRADE.get()));
		registration.addCraftingStation(RecipeTypes.STONECUTTING, new ItemStack(ModItems.STONECUTTER_UPGRADE.get()));
		registration.addCraftingStation(RecipeTypes.SMITHING, new ItemStack(ModItems.SMITHING_UPGRADE.get()));
		additionalCatalystRegistrar.accept(registration);
	}

	@Override
	public void registerRecipeTransferHandlers(IRecipeTransferRegistration registration) {
		IRecipeTransferHandlerHelper handlerHelper = registration.getTransferHelper();
		IStackHelper stackHelper = registration.getJeiHelpers().getStackHelper();
		registration.addRecipeTransferHandler(new JeiCraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<CraftingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public IRecipeType<RecipeHolder<CraftingRecipe>> getRecipeType() {
				return RecipeTypes.CRAFTING;
			}
		}, RecipeTypes.CRAFTING);

		registration.addRecipeTransferHandler(new JeiCraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<SmithingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public IRecipeType<RecipeHolder<SmithingRecipe>> getRecipeType() {
				return RecipeTypes.SMITHING;
			}
		}, RecipeTypes.SMITHING);
	}
}
