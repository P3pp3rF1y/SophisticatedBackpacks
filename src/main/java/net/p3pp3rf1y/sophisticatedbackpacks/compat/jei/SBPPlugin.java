package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.RecipeTypes;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.ingredients.subtypes.IIngredientSubtypeInterpreter;
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
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.compat.jei.ClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.jei.CraftingContainerRecipeTransferHandlerBase;
import net.p3pp3rf1y.sophisticatedcore.compat.jei.SettingsGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.jei.StorageGhostIngredientHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;

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
		IIngredientSubtypeInterpreter<ItemStack> backpackNbtInterpreter = (itemStack, context) -> {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(itemStack);
			return "{clothColor:" + wrapper.getMainColor() + ",borderColor:" + wrapper.getAccentColor() + "}";
		};
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.COPPER_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.IRON_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.GOLD_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.DIAMOND_BACKPACK.get(), backpackNbtInterpreter);
		registration.registerSubtypeInterpreter(VanillaTypes.ITEM_STACK, ModItems.NETHERITE_BACKPACK.get(), backpackNbtInterpreter);
	}

	@Override
	public void registerGuiHandlers(IGuiHandlerRegistration registration) {
		registration.addGuiContainerHandler(BackpackScreen.class, new IGuiContainerHandler<>() {
			@Override
			public List<Rect2i> getGuiExtraAreas(BackpackScreen gui) {
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
		registration.addRecipes(RecipeTypes.CRAFTING, ClientRecipeHelper.transformAllRecipesOfType(RecipeType.CRAFTING, BackpackUpgradeRecipe.class, ClientRecipeHelper::copyShapedRecipe));
	}

	@Override
	public void registerRecipeCatalysts(IRecipeCatalystRegistration registration) {
		registration.addRecipeCatalyst(new ItemStack(ModItems.CRAFTING_UPGRADE.get()), RecipeTypes.CRAFTING);
		registration.addRecipeCatalyst(new ItemStack(ModItems.STONECUTTER_UPGRADE.get()), RecipeTypes.STONECUTTING);
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
