package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraftforge.client.event.ColorHandlerEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.*;

public class ModItemColors {
	private ModItemColors() {}

	public static void registerItemColorHandlers(ColorHandlerEvent.Item event) {
		event.getItemColors().register((backpack, layer) -> {
			if (layer > 1 || !(backpack.getItem() instanceof BackpackItem)) {
				return -1;
			}
			return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(backpackWrapper -> {
				if (layer == 0) {
					return backpackWrapper.getMainColor();
				} else if (layer == 1) {
					return backpackWrapper.getAccentColor();
				}
				return -1;
			}).orElse(BackpackWrapper.DEFAULT_CLOTH_COLOR);
		}, BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}
}
