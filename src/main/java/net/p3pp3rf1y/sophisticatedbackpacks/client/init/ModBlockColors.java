package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.color.BlockColors;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTileEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.*;

public class ModBlockColors {
	private ModBlockColors() {}

	public static void init() {
		BlockColors blockColors = Minecraft.getInstance().getBlockColors();

		blockColors.register((state, blockDisplayReader, pos, tintIndex) -> {
			if (tintIndex < 0 || tintIndex > 1 || pos == null) {
				return -1;
			}
			return WorldHelper.getTile(blockDisplayReader, pos, BackpackTileEntity.class)
					.map(te -> tintIndex == 0 ? te.getBackpackWrapper().getClothColor() : te.getBackpackWrapper().getBorderColor())
					.orElse(getDefaultColor(tintIndex));
		}, BACKPACK.get(), IRON_BACKPACK.get(), GOLD_BACKPACK.get(), DIAMOND_BACKPACK.get(), NETHERITE_BACKPACK.get());
	}

	private static int getDefaultColor(int tintIndex) {
		return tintIndex == 0 ? BackpackWrapper.DEFAULT_CLOTH_COLOR : BackpackWrapper.DEFAULT_BORDER_COLOR;
	}
}
