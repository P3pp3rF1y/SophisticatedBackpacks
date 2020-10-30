package net.p3pp3rf1y.sophisticatedbackpacks.client.init;

import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.color.BlockColors;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks.*;

@OnlyIn(Dist.CLIENT)
public class ModBlockColors {
	private ModBlockColors() {}

	public static void init() {
		BlockColors blockColors = Minecraft.getInstance().getBlockColors();

		blockColors.register((state, blockDisplayReader, pos, tintIndex) -> tintIndex >= 0 && tintIndex <= 1 ? 13394234 : -1, BACKPACK, IRON_BACKPACK, GOLD_BACKPACK, DIAMOND_BACKPACK);
	}
}
