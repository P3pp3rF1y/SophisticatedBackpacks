package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.color.ItemColors;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.*;

public class ModItemColors {
	private ModItemColors() {}

	public static void init() {
		ItemColors itemColors = Minecraft.getInstance().getItemColors();

		itemColors.register((backpack, layer) -> {
			if (layer > 1 || !(backpack.getItem() instanceof BackpackItem)) {
				return -1;
			}
			return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(backpackWrapper -> {
				if (layer == 0) {
					return backpackWrapper.getClothColor();
				} else if (layer == 1) {
					return backpackWrapper.getBorderColor();
				}
				return -1;
			}).orElse(-1);
		}, BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}
}
