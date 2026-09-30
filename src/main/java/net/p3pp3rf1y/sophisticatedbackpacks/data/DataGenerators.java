package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.RegistrySetBuilder;
import net.minecraft.core.registries.Registries;
import net.minecraft.data.recipes.RecipeProvider;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class DataGenerators {
	private DataGenerators() {
	}

	public static void gatherData(GatherDataEvent.Client evt) {
		evt.createBlockAndItemTags((packOutput1, completableFuture) -> new BlockTagsProvider(packOutput1, completableFuture, SophisticatedBackpacks.MOD_ID) {
			@Override
			protected void addTags(HolderLookup.Provider pProvider) {
				// noop
			}
		}, (packOutput, lookupProvider, blockTagProvider) -> new ItemTagProvider(packOutput, lookupProvider));
		evt.createReloadableRegistryObjects(new RegistrySetBuilder().add(Registries.LOOT_TABLE, new BackpackLootTableProvider())
				.add(RecipeProvider.asBootstrap(BackpackRecipeProvider::new)));
		evt.createProvider(BackpackLootModifierProvider::new);
		evt.createProvider(BackpackModelProvider::new);
	}
}
