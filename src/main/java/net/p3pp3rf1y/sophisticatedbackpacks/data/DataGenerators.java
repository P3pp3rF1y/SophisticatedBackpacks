package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.neoforged.neoforge.common.data.BlockTagsProvider;
import net.neoforged.neoforge.data.event.GatherDataEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import java.util.concurrent.CompletableFuture;

public class DataGenerators {
	private DataGenerators() {}

	public static void gatherData(GatherDataEvent evt) {
		DataGenerator generator = evt.getGenerator();
		PackOutput packOutput = generator.getPackOutput();
		CompletableFuture<HolderLookup.Provider> registries = evt.getLookupProvider();
		BlockTagsProvider blockTagProvider = new BlockTagsProvider(packOutput, evt.getLookupProvider(), SophisticatedBackpacks.MOD_ID, evt.getExistingFileHelper()){
			@Override
			protected void addTags(HolderLookup.Provider pProvider) {
				//noop
			}
		};
		generator.addProvider(evt.includeServer(), blockTagProvider);
		generator.addProvider(evt.includeServer(), new ItemTagProvider(packOutput, evt.getLookupProvider(), blockTagProvider.contentsGetter(), evt.getExistingFileHelper()));
		generator.addProvider(evt.includeServer(), new SBLootTableProvider(packOutput, registries));
		generator.addProvider(evt.includeServer(), new SBLootModifierProvider(packOutput, registries));
		generator.addProvider(evt.includeServer(), new SBPRecipeProvider(packOutput, registries));
	}
}
