package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.data.event.GatherDataEvent;

public class DataGenerators {
	private DataGenerators() {}

	public static void gatherData(GatherDataEvent evt) {
		DataGenerator generator = evt.getGenerator();
		generator.addProvider(evt.includeServer(), new SBPBlockLootProvider(generator));
		generator.addProvider(evt.includeServer(), new SBPRecipeProvider(generator));
		generator.addProvider(evt.includeServer(), new SBPLootInjectProvider(generator));
	}
}
