package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.PackOutput;
import net.minecraftforge.data.event.GatherDataEvent;

public class DataGenerators {
	private DataGenerators() {}

	public static void gatherData(GatherDataEvent evt) {
		DataGenerator generator = evt.getGenerator();
		PackOutput packOutput = generator.getPackOutput();
		generator.addProvider(evt.includeServer(), new SBLootTableProvider(packOutput));
		generator.addProvider(evt.includeServer(), new SBLootModifierProvider(packOutput));
		generator.addProvider(evt.includeServer(), new SBPRecipeProvider(packOutput));
	}
}
