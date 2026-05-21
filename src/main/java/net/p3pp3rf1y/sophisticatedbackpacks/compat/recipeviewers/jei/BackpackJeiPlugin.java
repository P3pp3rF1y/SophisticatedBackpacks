package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.jei;

import mezz.jei.api.IModPlugin;
import mezz.jei.api.JeiPlugin;
import mezz.jei.api.constants.RecipeTypes;
import mezz.jei.api.constants.VanillaTypes;
import mezz.jei.api.gui.handlers.IGuiContainerHandler;
import mezz.jei.api.helpers.IStackHelper;
import mezz.jei.api.recipe.RecipeType;
import mezz.jei.api.recipe.transfer.IRecipeTransferHandlerHelper;
import mezz.jei.api.registration.*;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.bus.api.EventPriority;
import net.neoforged.neoforge.client.event.RecipesUpdatedEvent;
import net.neoforged.neoforge.common.NeoForge;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingRecipe;
import net.minecraft.world.item.crafting.RecipeHolder;
import net.minecraft.world.item.crafting.SmithingRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.BackpackRecipeViewerDisplays;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayContext;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.RecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.CraftingDisplayCatalogRecipeManagerPlugin;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.GroupedCraftingRecipeCategoryExtension;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.GroupedCraftingRecipeManagerPlugin;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiCraftingSpecExtensionRegistrar;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiCraftingContainerRecipeTransferHandlerBase;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiSettingsGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.SmithingSpecCategoryExtension;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.SmithingSpecRecipeManagerPlugin;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.JeiStorageGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.jei.subtypes.JeiSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.SubtypeInterpreters.getSubtypeInterpreters;

@SuppressWarnings("unused")
@JeiPlugin
public class BackpackJeiPlugin implements IModPlugin {
	private static Consumer<IRecipeCatalystRegistration> additionalCatalystRegistrar = registration -> {};
	private IRecipeViewerDisplayCatalog catalog = null;

	public BackpackJeiPlugin() {
		NeoForge.EVENT_BUS.addListener(EventPriority.HIGHEST, this::onRecipesUpdated);
	}

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
		IRecipeViewerDisplayCatalog catalog = getCatalog();
		registration.addRecipes(RecipeTypes.CRAFTING, catalog.getCraftingRecipes());
	}

	@Override
	public void registerVanillaCategoryExtensions(IVanillaCategoryExtensionRegistration registration) {
		GroupedCraftingRecipeCategoryExtension.registerOnce(registration);
		JeiCraftingSpecExtensionRegistrar.registerCraftingSpecExtensions(registration, this::getCatalog, stack -> stack.getItem() instanceof BackpackItem);
		registration.getSmithingCategory().addExtension(SmithingBackpackUpgradeRecipe.class,
				new SmithingSpecCategoryExtension<>(recipe -> getCatalog().getSmithingDisplaySpecReplacing(recipe).orElseThrow(), stack -> stack.getItem() instanceof BackpackItem));
	}

	@Override
	public void registerAdvanced(IAdvancedRegistration registration) {
		registration.addTypedRecipeManagerPlugin(RecipeTypes.CRAFTING, new GroupedCraftingRecipeManagerPlugin(() -> getCatalog().getGroupedCraftingSpecs(), BackpackJeiPlugin::canShowDyeUsagesFor, BackpackJeiPlugin::canShowDyeRecipesFor));
		registration.addTypedRecipeManagerPlugin(RecipeTypes.CRAFTING, new CraftingDisplayCatalogRecipeManagerPlugin(this::getCatalog, stack -> stack.getItem() instanceof BackpackItem));
		registration.addTypedRecipeManagerPlugin(RecipeTypes.SMITHING, new SmithingSpecRecipeManagerPlugin(this::getCatalog, BackpackRecipeViewerDisplays::needsSyntheticSmithingDisplay));
	}

	private static boolean canShowDyeUsagesFor(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem
				&& !stack.has(ModCoreDataComponents.MAIN_COLOR)
				&& !stack.has(ModCoreDataComponents.ACCENT_COLOR)
				&& !stack.has(ModCoreDataComponents.RENDER_INFO_TAG);
	}

	private IRecipeViewerDisplayCatalog getCatalog() {
		if (catalog == null) {
			catalog = createCatalog(getSubtypeInterpreters());
		}
		return catalog;
	}

	private static boolean canShowDyeRecipesFor(ItemStack stack) {
		if (!stack.is(ModItems.BACKPACK.get()) || !stack.has(ModCoreDataComponents.MAIN_COLOR) || !stack.has(ModCoreDataComponents.ACCENT_COLOR)) {
			return false;
		}

		int mainColor = stack.get(ModCoreDataComponents.MAIN_COLOR);
		int accentColor = stack.get(ModCoreDataComponents.ACCENT_COLOR);
		return mainColor == accentColor && isDyeColor(mainColor);
	}

	private static boolean isDyeColor(int color) {
		for (DyeColor dyeColor : DyeColor.values()) {
			if (dyeColor.getTextureDiffuseColor() == color) {
				return true;
			}
		}
		return false;
	}

	private void onRecipesUpdated(RecipesUpdatedEvent event) {
		catalog = null;
	}

	private static IRecipeViewerDisplayCatalog createCatalog(Map<Item, PropertyBasedSubtypeInterpreter> subtypeInterpreters) {
		IRecipeViewerDisplayCatalog catalog = new RecipeViewerDisplayCatalog();
		IRecipeViewerDisplayContext context = stack -> Optional.ofNullable(subtypeInterpreters.get(stack.getItem()));
		BackpackRecipeViewerDisplays.register(catalog, context);
		return catalog;
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
		registration.addRecipeTransferHandler(new JeiCraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<CraftingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public RecipeType<RecipeHolder<CraftingRecipe>> getRecipeType() {
				return RecipeTypes.CRAFTING;
			}
		}, RecipeTypes.CRAFTING);

		registration.addRecipeTransferHandler(new JeiCraftingContainerRecipeTransferHandlerBase<BackpackContainer, RecipeHolder<SmithingRecipe>>(handlerHelper, stackHelper) {
			@Override
			public Class<BackpackContainer> getContainerClass() {
				return BackpackContainer.class;
			}

			@Override
			public RecipeType<RecipeHolder<SmithingRecipe>> getRecipeType() {
				return RecipeTypes.SMITHING;
			}
		}, RecipeTypes.SMITHING);
	}
}
