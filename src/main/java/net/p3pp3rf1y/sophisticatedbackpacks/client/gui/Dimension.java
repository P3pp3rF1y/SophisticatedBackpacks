package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

public class Dimension {
	public static final Dimension SQUARE_256 = new Dimension(256, 256);
	public static final Dimension SQUARE_16 = new Dimension(16, 16);
	public static final Dimension SQUARE_12 = new Dimension(12, 12);
	public static final Dimension SQUARE_18 = new Dimension(18, 18);
	public static final Dimension EMPTY = new Dimension(0, 0);

	private final int width;
	private final int height;

	public Dimension(int width, int height) {
		this.width = width;
		this.height = height;
	}

	public int getWidth() {
		return width;
	}

	public int getHeight() {
		return height;
	}
}
