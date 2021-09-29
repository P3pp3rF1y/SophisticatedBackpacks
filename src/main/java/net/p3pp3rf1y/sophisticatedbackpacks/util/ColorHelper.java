package net.p3pp3rf1y.sophisticatedbackpacks.util;

public class ColorHelper {
	private ColorHelper() {}

	public static int getColor(float[] colorComponents) {
		int red = (int) (colorComponents[0] * 255);
		int green = (int) (colorComponents[1] * 255);
		int blue = (int) (colorComponents[2] * 255);

		return red << 16 | green << 8 | blue;
	}
}
