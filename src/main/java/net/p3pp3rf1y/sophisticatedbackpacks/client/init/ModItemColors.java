package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.color.ItemColors;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.BackpackWrapper;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.*;

@OnlyIn(Dist.CLIENT)
public class ModItemColors {
	private ModItemColors() {}

	public static void init() {
		ItemColors itemColors = Minecraft.getInstance().getItemColors();

		itemColors.register((backpack, layer) -> {
			if (layer > 1 || !(backpack.getItem() instanceof BackpackItem)) {
				return -1;
			}
			BackpackWrapper backpackWrapper = new BackpackWrapper(backpack, false);
			if (layer == 0) {
				return backpackWrapper.getClothColor();
			} else if (layer == 1) {
				return backpackWrapper.getBorderColor();
			}

			return -1;
		}, BACKPACK, IRON_BACKPACK, GOLD_BACKPACK, DIAMOND_BACKPACK);
	}
}
