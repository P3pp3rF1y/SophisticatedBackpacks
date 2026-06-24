package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.rei;

import dev.architectury.event.EventResult;
import me.shedaniel.math.Rectangle;
import me.shedaniel.rei.api.client.plugins.REIClientPlugin;
import me.shedaniel.rei.api.client.registry.category.CategoryRegistry;
import me.shedaniel.rei.api.client.registry.display.DisplayRegistry;
import me.shedaniel.rei.api.client.registry.entry.CollapsibleEntryRegistry;
import me.shedaniel.rei.api.client.registry.entry.EntryRegistry;
import me.shedaniel.rei.api.client.registry.screen.ExclusionZones;
import me.shedaniel.rei.api.client.registry.screen.ScreenRegistry;
import me.shedaniel.rei.api.client.registry.transfer.TransferHandlerRegistry;
import me.shedaniel.rei.api.common.category.CategoryIdentifier;
import me.shedaniel.rei.api.common.display.Display;
import me.shedaniel.rei.api.common.entry.EntryStack;
import me.shedaniel.rei.api.common.util.EntryStacks;
import me.shedaniel.rei.forge.REIPluginClient;
import me.shedaniel.rei.plugin.common.BuiltinPlugin;
import me.shedaniel.rei.plugin.common.displays.DefaultSmithingDisplay;
import me.shedaniel.rei.plugin.common.displays.crafting.DefaultCraftingDisplay;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.Identifier;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.BackpackRecipeViewerDisplays;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.IRecipeViewerDisplayContext;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.RecipeViewerDisplayCatalog;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.rei.*;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;

import java.util.*;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.SubtypeInterpreters.getSubtypeInterpreters;

@SuppressWarnings("unused")
@REIPluginClient
public class BackpackReiClientPlugin implements REIClientPlugin {
	private static Consumer<WorkstationRegistration> additionalWorkstations = registration -> {
	};
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
			if (screen == null) {
				return List.of();
			}

			return screen.getExtendedControlsRectangles().stream().map(r -> new Rectangle(r.getX(), r.getY(), r.getWidth(), r.getHeight())).toList();
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
	public void registerEntries(EntryRegistry registry) {
		ModItems.ITEMS.getEntries().stream().map(holder -> holder.get()).filter(BackpackItem.class::isInstance).map(BackpackItem.class::cast)
				.forEach(backpackItem -> getCreativeVariants(backpackItem).stream().filter(stack -> !registry.alreadyContain(EntryStacks.of(stack)))
						.forEach(stack -> registry.addEntry(EntryStacks.of(stack))));
	}

	@Override
	public void registerCollapsibleEntries(CollapsibleEntryRegistry registry) {
		ModItems.ITEMS.getEntries().stream().map(holder -> holder.get()).filter(BackpackItem.class::isInstance).map(BackpackItem.class::cast)
				.forEach(backpackItem -> {
					List<ItemStack> variants = getCreativeVariants(backpackItem);
					if (variants.size() > 1) {
						registry.group(getCollapseId(backpackItem), backpackItem.getName(backpackItem.getDefaultInstance()),
								variants.stream().map(EntryStacks::of).toList());
					}
				});
	}

	@Override
	public void registerDisplays(DisplayRegistry registry) {
		Map<Item, PropertyBasedSubtypeInterpreter> subtypeInterpreters = getSubtypeInterpreters();
		IRecipeViewerDisplayCatalog catalog = createCatalog(subtypeInterpreters);
		registry.registerGlobalDisplayGenerator(new GroupedCraftingReiDisplayGenerator(() -> catalog, BackpackReiClientPlugin::canShowDyeUsagesFor,
				BackpackReiClientPlugin::canShowDyeRecipesFor));
		registry.registerGlobalDisplayGenerator(new CraftingSpecReiDisplayGenerator(() -> catalog, stack -> stack.getItem() instanceof BackpackItem));
		registry.registerGlobalDisplayGenerator(new SmithingSpecReiDisplayGenerator(() -> catalog, stack -> stack.getItem() instanceof BackpackItem));
		registry.registerVisibilityPredicate((category, display) -> {
			if (display instanceof CraftingSpecReiDisplay) {
				return EventResult.pass();
			}
			if (display instanceof DefaultCraftingDisplay && display.getDisplayLocation().isPresent()
					&& catalog.getCraftingSpecs().stream().anyMatch(spec -> spec.replacedRecipeIds().contains(display.getDisplayLocation().get()))) {
				return EventResult.interruptFalse();
			}
			if (display instanceof DefaultSmithingDisplay smithingDisplay && smithingDisplay.getDisplayLocation().isPresent()) {
				String path = smithingDisplay.getDisplayLocation().get().getPath();
				if (!path.startsWith("backpack_smithing_upgrade_grouped/") && path.contains("netherite_backpack")) {
					return EventResult.interruptFalse();
				}
			}
			return EventResult.pass();
		});
	}

	private static boolean canShowDyeUsagesFor(ItemStack stack) {
		return stack.getItem() instanceof BackpackItem && !stack.has(ModCoreDataComponents.MAIN_COLOR) && !stack.has(ModCoreDataComponents.ACCENT_COLOR)
				&& !stack.has(ModCoreDataComponents.RENDER_DATA);
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

	private static IRecipeViewerDisplayCatalog createCatalog(Map<Item, PropertyBasedSubtypeInterpreter> subtypeInterpreters) {
		IRecipeViewerDisplayCatalog catalog = new RecipeViewerDisplayCatalog();
		IRecipeViewerDisplayContext context = stack -> Optional.ofNullable(subtypeInterpreters.get(stack.getItem()));
		BackpackRecipeViewerDisplays.register(catalog, context);
		return catalog;
	}

	private static List<ItemStack> getCreativeVariants(BackpackItem backpackItem) {
		List<ItemStack> variants = new ArrayList<>();
		backpackItem.addCreativeTabItems(variants::add);
		return variants;
	}

	private static Identifier getCollapseId(BackpackItem backpackItem) {
		return Identifier.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "rei_group/" + BuiltInRegistries.ITEM.getKey(backpackItem).getPath());
	}
}
