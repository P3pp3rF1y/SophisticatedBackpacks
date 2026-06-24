package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.neoforged.neoforge.client.event.RegisterColorHandlersEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.WorldHelper;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.*;

public class ModBlockColors {
	private ModBlockColors() {
	}

	public static void registerBlockColorHandlers(RegisterColorHandlersEvent.Block event) {
		event.register((state, blockDisplayReader, pos, tintIndex) -> {
			if (tintIndex < 0 || tintIndex > 1 || pos == null) {
				return -1;
			}
			return WorldHelper.getBlockEntity(blockDisplayReader, pos, BackpackBlockEntity.class)
					.map(be -> tintIndex == 0 ? be.getBackpackWrapper().getMainColor() : be.getBackpackWrapper().getAccentColor())
					.orElse(getDefaultColor(tintIndex));
		}, BACKPACK.get(), COPPER_BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}

	private static int getDefaultColor(int tintIndex) {
		return tintIndex == 0 ? BackpackWrapper.DEFAULT_MAIN_COLOR : BackpackWrapper.DEFAULT_ACCENT_COLOR;
	}
}
