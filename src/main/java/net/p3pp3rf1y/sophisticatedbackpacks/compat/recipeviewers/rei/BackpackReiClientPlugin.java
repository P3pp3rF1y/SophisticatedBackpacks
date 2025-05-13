package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.rei;

import me.shedaniel.math.Rectangle;
import me.shedaniel.rei.api.client.plugins.REIClientPlugin;
import me.shedaniel.rei.api.client.registry.category.CategoryRegistry;
import me.shedaniel.rei.api.client.registry.display.DisplayRegistry;
import me.shedaniel.rei.api.client.registry.screen.ExclusionZones;
import me.shedaniel.rei.api.client.registry.screen.ScreenRegistry;
import me.shedaniel.rei.api.client.registry.transfer.TransferHandlerRegistry;
import me.shedaniel.rei.api.common.category.CategoryIdentifier;
import me.shedaniel.rei.api.common.display.Display;
import me.shedaniel.rei.api.common.entry.EntryStack;
import me.shedaniel.rei.api.common.util.EntryStacks;
import me.shedaniel.rei.forge.REIPluginClient;
import me.shedaniel.rei.plugin.common.BuiltinPlugin;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.world.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.DyeRecipesMaker;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.rei.ReiCraftingContainerTransferHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.rei.ReiSettingsGhostIngredientHandler;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.rei.ReiStorageGhostIngredientHandler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

@SuppressWarnings("unused")
@REIPluginClient
public class BackpackReiClientPlugin implements REIClientPlugin {
	private static Consumer<WorkstationRegistration> additionalWorkstations = registration -> {};
	public static void addAdditionalWorkstations(Consumer<WorkstationRegistration> additionalWorkstations) {
		BackpackReiClientPlugin.additionalWorkstations = BackpackReiClientPlugin.additionalWorkstations.andThen(additionalWorkstations);
	}

	public static class WorkstationRegistration {
		private final CategoryRegistry registry;

		private WorkstationRegistration(CategoryRegistry registry) {
			this.registry = registry;
		}

		public void addWorkstations(CategoryIdentifier<? extends Display> id, Item... workstations) {
			registry.addWorkstations(id, Arrays.stream(workstations).map(EntryStacks::of).toArray(EntryStack[]::new));
		}
	}

	@Override
	public void registerExclusionZones(ExclusionZones zones) {
		zones.register(BackpackScreen.class, screen -> {
			List<Rect2i> ret = new ArrayList<>();
			screen.getUpgradeSlotsRectangle().ifPresent(ret::add);
			ret.addAll(screen.getUpgradeSettingsControl().getTabRectangles());
			screen.getSortButtonsRectangle().ifPresent(ret::add);
			return ret.stream().map(r -> new Rectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight())).toList();
		});

		zones.register(BackpackSettingsScreen.class, screen -> {
			if (screen == null || screen.getSettingsTabControl() == null) {
				return List.of();
			}

			return screen.getSettingsTabControl().getTabRectangles().stream().map(r -> new Rectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight())).toList();
		});
	}

	@Override
	public void registerTransferHandlers(TransferHandlerRegistry registry) {
		registry.register(ReiCraftingContainerTransferHandler.crafting(BackpackContainer.class));
		registry.register(ReiCraftingContainerTransferHandler.smithing(BackpackContainer.class));
	}

	@Override
	public void registerCategories(CategoryRegistry registry) {
		registry.addWorkstations(BuiltinPlugin.CRAFTING, EntryStacks.of(ModItems.CRAFTING_UPGRADE.get()));
		registry.addWorkstations(BuiltinPlugin.SMITHING, EntryStacks.of(ModItems.SMITHING_UPGRADE.get()));
		registry.addWorkstations(BuiltinPlugin.STONE_CUTTING, EntryStacks.of(ModItems.STONECUTTER_UPGRADE.get()));
		additionalWorkstations.accept(new WorkstationRegistration(registry));
	}

	@Override
	public void registerScreens(ScreenRegistry registry) {
		registry.registerDraggableStackVisitor(new ReiStorageGhostIngredientHandler<>(BackpackScreen.class));
		registry.registerDraggableStackVisitor(new ReiSettingsGhostIngredientHandler<>(BackpackSettingsScreen.class));
	}

	@Override
	public void registerDisplays(DisplayRegistry registry) {
		DyeRecipesMaker.getRecipes().forEach(registry::add);
	}
}