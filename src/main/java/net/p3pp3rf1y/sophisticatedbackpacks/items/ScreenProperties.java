package net.p3pp3rf1y.sophisticatedbackpacks.items;

public class ScreenProperties {
	private int slotsOnLine = 9;
	private int playerInventoryYOffset = 0;
	private int textureSize = 256;

	public int getSlotsOnLine() {
		return slotsOnLine;
	}

	public ScreenProperties setSlotsOnLine(int slotsOnLine) {
		this.slotsOnLine = slotsOnLine;
		return this;
	}

	public int getPlayerInventoryYOffset() {
		return playerInventoryYOffset;
	}

	public ScreenProperties setPlayerInventoryYOffset(int playerInventoryYOffset) {
		this.playerInventoryYOffset = playerInventoryYOffset;
		return this;
	}

	public int getTextureSize() {
		return textureSize;
	}

	public ScreenProperties setTextureSize(int textureSize) {
		this.textureSize = textureSize;
		return this;
	}
}
