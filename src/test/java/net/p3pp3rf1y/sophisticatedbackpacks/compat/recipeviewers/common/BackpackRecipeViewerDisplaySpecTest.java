package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common;

import net.minecraft.SharedConstants;
import net.minecraft.commands.Commands;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.LayeredRegistryAccess;
import net.minecraft.core.NonNullList;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.RegistryLayer;
import net.minecraft.server.ReloadableServerResources;
import net.minecraft.server.WorldLoader;
import net.minecraft.server.packs.repository.PackRepository;
import net.minecraft.server.packs.repository.ServerPacksSource;
import net.minecraft.server.packs.resources.CloseableResourceManager;
import net.minecraft.world.item.*;
import net.minecraft.world.item.crafting.*;
import net.minecraft.world.level.WorldDataConfiguration;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes.BackpackSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.BackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.crafting.SmithingBackpackUpgradeRecipe;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.*;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.junit.jupiter.api.Assertions.*;

@Tag("recipeViewerRegression")
class BackpackRecipeViewerDisplaySpecTest {
	@Test
	void tierUpgradeUsagePreservesFocusedBackpackComponents() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();
		ItemStack tintedBasicBackpack = tintedStack(ModItems.BACKPACK.get());

		List<CraftingDisplayVariant> usages = getCraftingUsagesFor(catalog, tintedBasicBackpack);

		assertEquals(2, usages.size());
		assertTrue(usages.stream().allMatch(usage -> ItemStack.isSameItemSameComponents(tintedBasicBackpack, usage.inputs().get(4))));
		assertTrue(usages.stream().anyMatch(usage -> ItemStack.isSameItemSameComponents(tintedStack(ModItems.COPPER_BACKPACK.get()), usage.firstOutput())));
		assertTrue(usages.stream().anyMatch(usage -> ItemStack.isSameItemSameComponents(tintedStack(ModItems.IRON_BACKPACK.get()), usage.firstOutput())));
	}

	@Test
	void tierUpgradeRecipePreservesFocusedResultComponents() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();
		ItemStack tintedIronBackpack = tintedStack(ModItems.IRON_BACKPACK.get());

		List<CraftingDisplayVariant> recipes = getCraftingRecipesFor(catalog, tintedIronBackpack);

		assertEquals(2, recipes.size());
		assertTrue(recipes.stream().anyMatch(recipe -> ItemStack.isSameItemSameComponents(tintedStack(ModItems.BACKPACK.get()), recipe.inputs().get(4))));
		assertTrue(recipes.stream().anyMatch(recipe -> ItemStack.isSameItemSameComponents(tintedStack(ModItems.COPPER_BACKPACK.get()), recipe.inputs().get(4))));
		assertTrue(recipes.stream().allMatch(recipe -> ItemStack.isSameItemSameComponents(tintedIronBackpack, recipe.firstOutput())));
	}

	@Test
	void ironTierRecipeCanComeFromBasicAndCopper() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();
		ItemStack ironBackpack = new ItemStack(ModItems.IRON_BACKPACK.get());

		List<CraftingDisplayVariant> recipes = getCraftingRecipesFor(catalog, ironBackpack);

		assertEquals(2, recipes.size());
		assertTrue(recipes.stream().anyMatch(recipe -> ItemStack.isSameItem(new ItemStack(ModItems.BACKPACK.get()), recipe.inputs().get(4))));
		assertTrue(recipes.stream().anyMatch(recipe -> ItemStack.isSameItem(new ItemStack(ModItems.COPPER_BACKPACK.get()), recipe.inputs().get(4))));
	}

	@Test
	void tierUsageChainCanReachNetheriteSmithingUpgrade() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();

		ItemStack ironBackpack = new ItemStack(ModItems.IRON_BACKPACK.get());
		ItemStack goldBackpack = getCraftingUsagesFor(catalog, ironBackpack).getFirst().firstOutput();
		ItemStack diamondBackpack = getCraftingUsagesFor(catalog, goldBackpack).getFirst().firstOutput();
		List<SmithingDisplayVariant> smithingUsages = getSmithingUsagesFor(catalog, diamondBackpack);

		assertEquals(1, smithingUsages.size());
		assertTrue(ItemStack.isSameItem(new ItemStack(ModItems.NETHERITE_BACKPACK.get()), smithingUsages.getFirst().result()));
	}

	@Test
	void smithingUsagePreservesFocusedDiamondBackpackComponents() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();
		ItemStack tintedDiamondBackpack = tintedStack(ModItems.DIAMOND_BACKPACK.get());

		List<SmithingDisplayVariant> usages = getSmithingUsagesFor(catalog, tintedDiamondBackpack);

		assertEquals(1, usages.size());
		assertSameStack(tintedDiamondBackpack, usages.getFirst().base());
		assertSameStack(tintedStack(ModItems.NETHERITE_BACKPACK.get()), usages.getFirst().result());
	}

	@Test
	void smithingRecipePreservesFocusedNetheriteBackpackComponents() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();
		ItemStack tintedNetheriteBackpack = tintedStack(ModItems.NETHERITE_BACKPACK.get());

		List<SmithingDisplayVariant> recipes = getSmithingRecipesFor(catalog, tintedNetheriteBackpack);

		assertEquals(1, recipes.size());
		assertSameStack(tintedStack(ModItems.DIAMOND_BACKPACK.get()), recipes.getFirst().base());
		assertSameStack(tintedNetheriteBackpack, recipes.getFirst().result());
	}

	@Test
	void smithingSyntheticRecipeHolderUsesFocusedBaseAndResult() {
		SmithingDisplaySpec spec = getSmithingSpecForResult(createCatalog(), new ItemStack(ModItems.NETHERITE_BACKPACK.get()));
		ItemStack tintedNetheriteBackpack = tintedStack(ModItems.NETHERITE_BACKPACK.get());
		SmithingDisplayVariant focusedVariant = spec.getRecipesFor(tintedNetheriteBackpack).getFirst();

		RecipeHolder<SmithingRecipe> recipeHolder = spec.recipeHolder(focusedVariant);

		assertTrue(recipeHolder.value().isBaseIngredient(tintedStack(ModItems.DIAMOND_BACKPACK.get())));
		assertFalse(recipeHolder.value().isBaseIngredient(tintedStack(ModItems.IRON_BACKPACK.get())));
		assertSameStack(tintedNetheriteBackpack, recipeHolder.value().getResultItem(null));
	}

	@Test
	void globalTierUpgradeDisplaysDoNotIncludeGeneratedTintedVariants() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();

		List<CraftingDisplayVariant> globalVariants = catalog.getGlobalCraftingDisplays().stream().flatMap(view -> view.variants().stream()).toList();

		assertFalse(globalVariants.isEmpty());
		assertTrue(globalVariants.stream().noneMatch(variant -> isTinted(variant.firstOutput())));
		assertTrue(globalVariants.stream().noneMatch(variant -> variant.inputs().stream().anyMatch(BackpackRecipeViewerDisplaySpecTest::isTinted)));
	}

	@Test
	void globalSmithingDisplaysDoNotIncludeGeneratedTintedVariants() {
		IRecipeViewerDisplayCatalog catalog = createCatalog();

		List<SmithingDisplayVariant> globalVariants = catalog.getGlobalSmithingDisplays().stream().flatMap(view -> view.variants().stream()).toList();

		assertFalse(globalVariants.isEmpty());
		assertTrue(globalVariants.stream().noneMatch(variant -> isTinted(variant.base())));
		assertTrue(globalVariants.stream().noneMatch(variant -> isTinted(variant.result())));
	}

	@Test
	void focusedHigherTierSingleColorDyeRecipeNarrowsDyeInputAndResult() {
		SingleColorDyeRecipeSpec ironBackpackDyeSpec = createCatalog().getGroupedCraftingSpecs().stream()
				.filter(SingleColorDyeRecipeSpec.class::isInstance)
				.map(SingleColorDyeRecipeSpec.class::cast)
				.filter(spec -> spec.sourceStacks().stream().anyMatch(stack -> stack.is(ModItems.IRON_BACKPACK.get())))
				.findFirst()
				.orElseThrow();
		ItemStack redIronBackpack = new ItemStack(ModItems.IRON_BACKPACK.get());
		BackpackItem.setColors(redIronBackpack, DyeColor.RED.getTextureDiffuseColor(), DyeColor.RED.getTextureDiffuseColor());
		redIronBackpack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, 54);
		redIronBackpack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, 2);

		List<RecipeHolder<GroupedCraftingRecipe>> focusedRecipes = ironBackpackDyeSpec.getRecipesFor(redIronBackpack);

		assertEquals(1, focusedRecipes.size());
		GroupedCraftingRecipe focusedRecipe = focusedRecipes.getFirst().value();
		assertEquals(1, focusedRecipe.getVariants().size());
		assertEquals(2, focusedRecipe.getInputSlots().size());
		assertEquals(1, focusedRecipe.getInputSlots().get(1).size());
		assertSameStack(new ItemStack(DyeItem.byColor(DyeColor.RED)), focusedRecipe.getInputSlots().get(1).getFirst());
		assertSameStack(redIronBackpack, focusedRecipe.getResultStacks().getFirst());
	}

	private static ItemStack tintedStack(Item item) {
		ItemStack stack = new ItemStack(item);
		BackpackItem.setColors(stack, 0x336699, 0x99CC33);
		return stack;
	}

	private static boolean isTinted(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			return false;
		}
		BackpackWrapper wrapper = (BackpackWrapper) BackpackWrapper.fromStack(stack);
		return wrapper.getMainColor() != BackpackWrapper.DEFAULT_MAIN_COLOR || wrapper.getAccentColor() != BackpackWrapper.DEFAULT_ACCENT_COLOR;
	}

	private static IRecipeViewerDisplayCatalog createCatalog() {
		IRecipeViewerDisplayCatalog catalog = new RecipeViewerDisplayCatalog();
		BackpackSubtypeInterpreter subtypeInterpreter = new BackpackSubtypeInterpreter();
		IRecipeViewerDisplayContext context = stack -> stack.getItem() instanceof BackpackItem ? Optional.of(subtypeInterpreter) : Optional.empty();
		try (TestRecipeResources.LoadedResources resources = TestRecipeResources.load(); MockedStatic<ClientRecipeHelper> clientRecipeHelper = Mockito.mockStatic(ClientRecipeHelper.class, Mockito.CALLS_REAL_METHODS)) {
			mockClientRecipeHelper(clientRecipeHelper, resources);
			BackpackRecipeViewerDisplays.register(catalog, context);
		}
		return catalog;
	}

	@SuppressWarnings({"unchecked", "rawtypes"})
	private static void mockClientRecipeHelper(MockedStatic<ClientRecipeHelper> clientRecipeHelper, TestRecipeResources.LoadedResources resources) {
		RecipeManager recipeManager = resources.recipeManager();
		clientRecipeHelper.when(() -> ClientRecipeHelper.transformAllRecipeHoldersOfTypeIntoMultiple(Mockito.any(), Mockito.any(), Mockito.any())).thenAnswer(invocation -> {
			RecipeType recipeType = invocation.getArgument(0);
			Class recipeClass = invocation.getArgument(1);
			return ClientRecipeHelper.transformAllRecipeHoldersOfTypeIntoMultiple(recipeManager, recipeType, recipeClass, invocation.getArgument(2));
		});
		clientRecipeHelper.when(() -> ClientRecipeHelper.transformAllRecipeHoldersOfType(Mockito.any(), Mockito.any(), Mockito.any())).thenAnswer(invocation -> {
			RecipeType recipeType = invocation.getArgument(0);
			Class recipeClass = invocation.getArgument(1);
			return ClientRecipeHelper.transformAllRecipeHoldersOfType(recipeManager, recipeType, recipeClass, invocation.getArgument(2));
		});
		clientRecipeHelper.when(() -> ClientRecipeHelper.assemble(Mockito.any(), Mockito.any())).thenAnswer(invocation -> assembleRecipe(invocation.getArgument(0), invocation.getArgument(1), resources.registryLookup()));
		clientRecipeHelper.when(() -> ClientRecipeHelper.getResultItem(Mockito.any())).thenAnswer(invocation -> ClientRecipeHelper.getResultItem(invocation.getArgument(0), resources.registryLookup()));
	}

	private static ItemStack assembleRecipe(Recipe<CraftingInput> recipe, CraftingInput input, HolderLookup.Provider registryLookup) {
		if (recipe instanceof BackpackUpgradeRecipe) {
			ItemStack result = ClientRecipeHelper.getResultItem(recipe, registryLookup).copy();
			for (int slot = 0; slot < input.size(); slot++) {
				ItemStack slotStack = input.getItem(slot);
				if (slotStack.getItem() instanceof BackpackItem) {
					result.applyComponents(slotStack.getComponents());
					return result;
				}
			}
		}
		return ClientRecipeHelper.assemble(recipe, input, registryLookup);
	}

	private static List<RecipeHolder<?>> createRecipeHolders() {
		return List.of(
				new RecipeHolder<>(ResourceLocation.parse("test:backpack_to_iron_backpack"), backpackUpgradeRecipe(ModItems.BACKPACK.get(), ModItems.IRON_BACKPACK.get())),
				new RecipeHolder<>(ResourceLocation.parse("test:copper_backpack_to_iron_backpack"), backpackUpgradeRecipe(ModItems.COPPER_BACKPACK.get(), ModItems.IRON_BACKPACK.get())),
				new RecipeHolder<>(ResourceLocation.parse("test:iron_backpack_to_gold_backpack"), backpackUpgradeRecipe(ModItems.IRON_BACKPACK.get(), ModItems.GOLD_BACKPACK.get())),
				new RecipeHolder<>(ResourceLocation.parse("test:gold_backpack_to_diamond_backpack"), backpackUpgradeRecipe(ModItems.GOLD_BACKPACK.get(), ModItems.DIAMOND_BACKPACK.get())),
				new RecipeHolder<>(ResourceLocation.parse("test:diamond_backpack_to_netherite_backpack"), smithingBackpackUpgradeRecipe())
		);
	}

	private static BackpackUpgradeRecipe backpackUpgradeRecipe(Item sourceItem, Item resultItem) {
		NonNullList<Ingredient> ingredients = NonNullList.withSize(9, Ingredient.EMPTY);
		ingredients.set(4, Ingredient.of(sourceItem));
		return new BackpackUpgradeRecipe(new ShapedRecipe("", CraftingBookCategory.MISC, new ShapedRecipePattern(3, 3, ingredients, Optional.empty()), new ItemStack(resultItem)));
	}

	private static SmithingBackpackUpgradeRecipe smithingBackpackUpgradeRecipe() {
		return new SmithingBackpackUpgradeRecipe(new SmithingTransformRecipe(Ingredient.of(Items.NETHERITE_UPGRADE_SMITHING_TEMPLATE), Ingredient.of(ModItems.DIAMOND_BACKPACK.get()), Ingredient.of(Items.NETHERITE_INGOT), new ItemStack(ModItems.NETHERITE_BACKPACK.get())));
	}

	private static List<CraftingDisplayVariant> getCraftingUsagesFor(IRecipeViewerDisplayCatalog catalog, ItemStack stack) {
		return catalog.getCraftingSpecs().stream().flatMap(spec -> spec.getUsagesFor(stack).stream()).toList();
	}

	private static List<CraftingDisplayVariant> getCraftingRecipesFor(IRecipeViewerDisplayCatalog catalog, ItemStack stack) {
		return catalog.getCraftingSpecs().stream().flatMap(spec -> spec.getRecipesFor(stack).stream()).toList();
	}

	private static List<SmithingDisplayVariant> getSmithingUsagesFor(IRecipeViewerDisplayCatalog catalog, ItemStack stack) {
		return catalog.getSmithingUsagesFor(stack).stream().flatMap(view -> view.variants().stream()).toList();
	}

	private static List<SmithingDisplayVariant> getSmithingRecipesFor(IRecipeViewerDisplayCatalog catalog, ItemStack stack) {
		return catalog.getSmithingRecipesFor(stack).stream().flatMap(view -> view.variants().stream()).toList();
	}

	private static SmithingDisplaySpec getSmithingSpecForResult(IRecipeViewerDisplayCatalog catalog, ItemStack stack) {
		return catalog.getSmithingRecipesFor(stack).getFirst().spec();
	}

	private static void assertSameStack(ItemStack expected, ItemStack actual) {
		assertTrue(ItemStack.isSameItemSameComponents(expected, actual), "Expected " + expected + " but got " + actual);
	}

	private final static class TestRecipeResources {
		private TestRecipeResources() {
		}

		private static LoadedResources load() {
			SharedConstants.tryDetectVersion();
			Bootstrap.bootStrap();

			ExecutorService backgroundExecutor = Executors.newFixedThreadPool(2);
			Executor gameExecutor = Runnable::run;
			try {
				PackRepository packRepository = ServerPacksSource.createVanillaTrustedRepository();
				WorldLoader.PackConfig packConfig = new WorldLoader.PackConfig(packRepository, WorldDataConfiguration.DEFAULT, false, false);
				WorldLoader.InitConfig initConfig = new WorldLoader.InitConfig(packConfig, Commands.CommandSelection.INTEGRATED, 0);

				return WorldLoader.load(
						initConfig,
						context -> new WorldLoader.DataLoadOutput<>(UnitCookie.INSTANCE, context.datapackDimensions()),
						(resourceManager, resources, registries, cookie) -> new LoadedResources(resourceManager, resources, registries),
						backgroundExecutor,
						gameExecutor
				).join();
			} finally {
				backgroundExecutor.shutdown();
			}
		}

		private enum UnitCookie {
			INSTANCE
		}

		private record LoadedResources(CloseableResourceManager resourceManager, ReloadableServerResources serverResources, LayeredRegistryAccess<RegistryLayer> registries) implements AutoCloseable {
			private RecipeManager recipeManager() {
				return serverResources.getRecipeManager();
			}

			private HolderLookup.Provider registryLookup() {
				return serverResources.getRegistryLookup();
			}

			@Override
			public void close() {
				resourceManager.close();
			}
		}
	}
}
