package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public class BackpackLootTableProvider extends LootTableProvider {
	BackpackLootTableProvider(PackOutput packOutput, CompletableFuture<HolderLookup.Provider> registries) {
		super(packOutput, BackpackInjectLootSubProvider.ALL_TABLES, List.of(new SubProviderEntry(BackpackBlockLootSubProvider::new, LootContextParamSets.BLOCK),
				new SubProviderEntry(BackpackInjectLootSubProvider::new, LootContextParamSets.CHEST)), registries);
	}
}
