package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.neoforged.neoforge.client.event.RegisterColorHandlersEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.*;

public class ModItemColors {
	private ModItemColors() {
	}

	public static void registerItemColorHandlers(RegisterColorHandlersEvent.Item event) {
		event.register((backpack, layer) -> {
			if (layer > 1 || !(backpack.getItem() instanceof BackpackItem)) {
				return -1;
			}
			if (layer == 0) {
				return BackpackItem.getMainColor(backpack);
			} else if (layer == 1) {
				return BackpackItem.getAccentColor(backpack);
			}
			return -1;
		}, BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}
}
