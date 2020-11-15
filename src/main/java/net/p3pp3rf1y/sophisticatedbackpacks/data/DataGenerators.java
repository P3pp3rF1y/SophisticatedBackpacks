package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent;

public class DataGenerators {
	private DataGenerators() {}

	public static void gatherData(GatherDataEvent evt) {
		DataGenerator generator = evt.getGenerator();
		generator.addProvider(new BlockLoot(generator));
		generator.addProvider(new Recipes(generator));
	}
}
