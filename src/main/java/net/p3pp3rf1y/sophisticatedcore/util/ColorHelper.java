package net.p3pp3rf1y.sophisticatedcore.util;

import net.minecraft.world.item.DyeColor;

import java.util.List;

public class ColorHelper {
	private ColorHelper() {}

	public static int getColor(float[] colorComponents) {
		int red = (int) (colorComponents[0] * 255);
		int green = (int) (colorComponents[1] * 255);
		int blue = (int) (colorComponents[2] * 255);

		return red << 16 | green << 8 | blue;
	}

	public static int calculateColor(int baseColor, int defaultColor, List<DyeColor> dyes) {
		if (dyes.isEmpty()) {
			return baseColor;
		}

		int[] rgb = new int[3];
		int sumMaxComponent = 0;
		int numberOfColors = 0;
		if (baseColor != defaultColor) {
			float baseRed = (baseColor >> 16 & 255);
			float baseGreen = (baseColor >> 8 & 255);
			float baseBlue = (baseColor & 255);
			sumMaxComponent = (int) (sumMaxComponent + Math.max(baseRed, Math.max(baseGreen, baseBlue)));
			rgb[0] = (int) (rgb[0] + baseRed);
			rgb[1] = (int) (rgb[1] + baseGreen);
			rgb[2] = (int) (rgb[2] + baseBlue);
			++numberOfColors;
		}

		for (DyeColor dye : dyes) {
			float[] dyeRgb = dye.getTextureDiffuseColors();
			int dyeRed = (int) (dyeRgb[0] * 255.0F);
			int dyeGreen = (int) (dyeRgb[1] * 255.0F);
			int dyeBlue = (int) (dyeRgb[2] * 255.0F);
			sumMaxComponent += Math.max(dyeRed, Math.max(dyeGreen, dyeBlue));
			rgb[0] += dyeRed;
			rgb[1] += dyeGreen;
			rgb[2] += dyeBlue;
			++numberOfColors;
		}

		int avgRed = rgb[0] / numberOfColors;
		int avgGreen = rgb[1] / numberOfColors;
		int avgBlue = rgb[2] / numberOfColors;
		float avgMaxComponent = (float) sumMaxComponent / (float) numberOfColors;
		float maxAvgComponent = Math.max(avgRed, Math.max(avgGreen, avgBlue));
		avgRed = (int) (avgRed * avgMaxComponent / maxAvgComponent);
		avgGreen = (int) (avgGreen * avgMaxComponent / maxAvgComponent);
		avgBlue = (int) (avgBlue * avgMaxComponent / maxAvgComponent);
		int finalColor = (avgRed << 8) + avgGreen;
		finalColor = (finalColor << 8) + avgBlue;

		return finalColor;
	}
}
