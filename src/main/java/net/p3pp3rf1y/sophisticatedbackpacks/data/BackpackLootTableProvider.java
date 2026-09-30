package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;

import java.util.List;

public class BackpackLootTableProvider extends LootTableProvider {
	BackpackLootTableProvider() {
		super(BackpackInjectLootSubProvider.ALL_TABLES, List.of(new SubProviderEntry(BackpackBlockLootSubProvider::new, LootContextParamSets.BLOCK),
				new SubProviderEntry(BackpackInjectLootSubProvider::new, LootContextParamSets.CHEST)));
	}
}
