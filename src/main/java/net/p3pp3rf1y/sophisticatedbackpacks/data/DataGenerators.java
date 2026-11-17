package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.core.registries.Registries;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.neoforged.neoforge.data.event.GatherDataRegistryEntriesEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class DataGenerators {
	private DataGenerators() {
	}

	public static void gatherRegistryEntries(GatherDataRegistryEntriesEvent evt) {
		evt.add(Registries.LOOT_TABLE, new BackpackLootTableProvider()).recipe(BackpackRecipeProvider::new);
	}

	public static void gatherData(GatherDataEvent.Client evt) {
		evt.createBlockAndItemTags((packOutput1, completableFuture) -> new BlockTagsProvider(packOutput1, completableFuture, SophisticatedBackpacks.MOD_ID) {
			@Override
			protected void addTags(HolderLookup.Provider pProvider) {
				// noop
			}
		}, (packOutput, lookupProvider, blockTagProvider) -> new ItemTagProvider(packOutput, lookupProvider));
		evt.createProvider(BackpackLootModifierProvider::new);
		evt.createProvider(BackpackModelProvider::new);
	}
}
