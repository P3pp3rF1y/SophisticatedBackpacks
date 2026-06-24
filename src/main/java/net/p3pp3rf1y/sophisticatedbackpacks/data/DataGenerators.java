package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class DataGenerators {
	private DataGenerators() {
	}

	public static void gatherData(GatherDataEvent.Client evt) {
		evt.createBlockAndItemTags(
				(packOutput1, completableFuture) -> new BlockTagsProvider(packOutput1, evt.getLookupProvider(), SophisticatedBackpacks.MOD_ID) {
					@Override
					protected void addTags(HolderLookup.Provider pProvider) {
						// noop
					}
				}, ItemTagProvider::new);
		evt.createProvider(BackpackLootTableProvider::new);
		evt.createProvider(BackpackLootModifierProvider::new);
		evt.createProvider(BackpackRecipeProvider.Runner::new);
		evt.createProvider(BackpackModelProvider::new);
	}
}
