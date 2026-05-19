package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.emi;

import dev.emi.emi.api.EmiEntrypoint;
import dev.emi.emi.api.EmiPlugin;
import dev.emi.emi.api.EmiRegistry;
import dev.emi.emi.api.recipe.EmiRecipeCategory;
import dev.emi.emi.api.recipe.VanillaEmiRecipeCategories;
import dev.emi.emi.api.stack.EmiStack;
import dev.emi.emi.api.widget.Bounds;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.BackpackRecipeViewerDisplays;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayContext;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.RecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.CraftingSpecEmiRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiGridMenuInfo;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.GroupedCraftingEmiRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiSettingsGhostDragDropHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.SmithingSpecEmiRecipe;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiStorageGhostDragDropHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.comparison.EmiSubtypeInterpreter;

import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.SubtypeInterpreters.getSubtypeInterpreters;

@EmiEntrypoint
public class BackpackEmiPlugin implements EmiPlugin {
	private static Consumer<WorkstationRegistration> additionalWorkstations = registrar -> {};
	public static void addAdditionalWorkstations(Consumer<WorkstationRegistration> additionalWorkstations) {
		BackpackEmiPlugin.additionalWorkstations = BackpackEmiPlugin.additionalWorkstations.andThen(additionalWorkstations);
	}

	public static class WorkstationRegistration {
		private final EmiRegistry registry;

		private WorkstationRegistration(EmiRegistry registry) {
			this.registry = registry;
		}

		public void addWorkstation(ResourceLocation id, Block icon, Item workstation) {
			addWorkstation(new EmiRecipeCategory(id, EmiStack.of(icon)), workstation);
		}

		public void addWorkstation(EmiRecipeCategory category, Item workstation) {
			addWorkstation(category, EmiStack.of(workstation));
		}

		public void addWorkstation(EmiRecipeCategory category, EmiStack workstation) {
			registry.addWorkstation(category, workstation);
		}
	}

	@Override
	public void register(EmiRegistry registry) {
		registerGuiHandlers(registry);
		registerRecipes(registry);
		registerDefaultComparisons(registry);
		registerRecipeHandlers(registry);
		registerWorkstations(registry);
	}

	private void registerDefaultComparisons(EmiRegistry registry) {
		getSubtypeInterpreters()
				.forEach((item, subtypeInterpreter) -> registry.setDefaultComparison(EmiStack.of(item), EmiSubtypeInterpreter.of(subtypeInterpreter)));
	}

	private void registerGuiHandlers(EmiRegistry registry) {
		registry.addExclusionArea(BackpackScreen.class, (screen, consumer) -> {
			//noinspection ConstantValue
			if (screen == null || screen.getUpgradeSettingsControl() == null) {
				return;
			}
			screen.getUpgradeSlotsRectangle().ifPresent(r -> consumer.accept(new Bounds(r.getX(), r.getY(), r.getWidth(), r.getHeight())));
			screen.getUpgradeSettingsControl().getTabRectangles().forEach(r -> consumer.accept(new Bounds(r.getX(), r.getY(), r.getWidth(), r.getHeight())));
			screen.getSortButtonsRectangle().ifPresent(r -> consumer.accept(new Bounds(r.getX(), r.getY(), r.getWidth(), r.getHeight())));
		});

		registry.addExclusionArea(BackpackSettingsScreen.class, (screen, consumer) -> {
			if (screen == null) { // Due to how Emi collects the exclusion area this can be null
				return;
			}
			screen.getExtendedControlsRectangles().forEach(r -> consumer.accept(new Bounds(r.getX(), r.getY(), r.getWidth(), r.getHeight())));
		});

		registry.addDragDropHandler(BackpackScreen.class, new EmiStorageGhostDragDropHandler<>());
		registry.addDragDropHandler(BackpackSettingsScreen.class, new EmiSettingsGhostDragDropHandler<>());
	}

	private void registerRecipes(EmiRegistry registry) {
		Map<Item, PropertyBasedSubtypeInterpreter> subtypeInterpreters = getSubtypeInterpreters();
		IRecipeViewerDisplayCatalog catalog = createCatalog(subtypeInterpreters);
		registry.removeRecipes(recipe -> recipe.getBackingRecipe() != null && catalog.replacesCraftingRecipe(recipe.getBackingRecipe()));
		catalog.getGroupedCraftingSpecs().stream()
				.flatMap(spec -> spec.getAllDisplays().stream())
				.flatMap(recipeHolder -> GroupedCraftingEmiRecipe.ofGroupedUsageAndFocusedRecipes(recipeHolder).stream())
				.forEach(registry::addRecipe);
		catalog.getCraftingRecipes().stream()
				.filter(recipeHolder -> !catalog.replacesCraftingRecipe(recipeHolder))
				.map(recipeHolder -> EmiClientRecipeHelper.wrapSyntheticShapedRecipe(recipeHolder.id(), recipeHolder.value()))
				.forEach(registry::addRecipe);
		registry.removeRecipes(recipe -> recipe.getBackingRecipe() != null && recipe.getBackingRecipe().value() instanceof SmithingBackpackUpgradeRecipe);

		catalog.getCraftingSpecs().stream()
				.flatMap(spec -> CraftingSpecEmiRecipe.ofGroupedUsageAndFocusedRecipes(spec).stream())
				.forEach(registry::addRecipe);

		catalog.getSmithingSpecs().stream()
				.flatMap(spec -> SmithingSpecEmiRecipe.ofGroupedUsageAndFocusedRecipes(spec).stream())
				.forEach(registry::addRecipe);
	}

	private static IRecipeViewerDisplayCatalog createCatalog(Map<Item, PropertyBasedSubtypeInterpreter> subtypeInterpreters) {
		IRecipeViewerDisplayCatalog catalog = new RecipeViewerDisplayCatalog();
		IRecipeViewerDisplayContext context = stack -> Optional.ofNullable(subtypeInterpreters.get(stack.getItem()));
		BackpackRecipeViewerDisplays.register(catalog, context);
		return catalog;
	}

	private void registerRecipeHandlers(EmiRegistry registry) {
		registry.addRecipeHandler(ModItems.BACKPACK_CONTAINER_TYPE.get(), EmiGridMenuInfo.crafting());
		registry.addRecipeHandler(ModItems.BACKPACK_CONTAINER_TYPE.get(), EmiGridMenuInfo.smithing());
	}

	private void registerWorkstations(EmiRegistry registry) {
		registry.addWorkstation(VanillaEmiRecipeCategories.CRAFTING, EmiStack.of(ModItems.CRAFTING_UPGRADE.get()));
		registry.addWorkstation(VanillaEmiRecipeCategories.STONECUTTING, EmiStack.of(ModItems.STONECUTTER_UPGRADE.get()));
		registry.addWorkstation(VanillaEmiRecipeCategories.SMITHING, EmiStack.of(ModItems.SMITHING_UPGRADE.get()));

		additionalWorkstations.accept(new WorkstationRegistration(registry));
	}

}
