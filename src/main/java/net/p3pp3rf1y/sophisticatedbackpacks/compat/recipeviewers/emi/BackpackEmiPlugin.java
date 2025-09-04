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
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.DyeRecipesMaker;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiClientRecipeHelper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiGridMenuInfo;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiSettingsGhostDragDropHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.EmiStorageGhostDragDropHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.emi.comparison.EmiSubtypeInterpreter;

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
		registry.addDragDropHandler(SettingsScreen.class, new EmiSettingsGhostDragDropHandler<>());
	}

	private void registerRecipes(EmiRegistry registry) {
		DyeRecipesMaker.getRecipes(EmiClientRecipeHelper::wrapSyntheticShapedRecipe).forEach(registry::addRecipe);
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