package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.PackOutput;
import net.minecraft.data.loot.LootTableProvider;
import net.minecraft.world.level.storage.loot.parameters.LootContextParamSets;

import java.util.List;

public class SBLootTableProvider extends LootTableProvider {
	SBLootTableProvider(PackOutput packOutput) {
		super(packOutput, SBInjectLootSubProvider.ALL_TABLES,
				List.of(
						new SubProviderEntry(SBPBlockLootSubProvider::new, LootContextParamSets.BLOCK),
						new SubProviderEntry(SBInjectLootSubProvider::new, LootContextParamSets.CHEST)
				)
		);
	}
}
