package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.util.ResourceLocation;

public class TextureBlitData {
	private final ResourceLocation textureName;
	private final int xOffset;
	private final int yOffset;
	private final int textureWidth;
	private final int textureHeight;
	private final int u;
	private final int v;
	private final int width;
	private final int height;

	public TextureBlitData(ResourceLocation textureName, int width, int height) {
		this(textureName, width, height, 0, 0, width, height);
	}

	public TextureBlitData(ResourceLocation textureName, int textureWidth, int textureHeight, int u, int v, int width, int height) {
		this(textureName, 0, 0, textureWidth, textureHeight, u, v, width, height);
	}

	public TextureBlitData(ResourceLocation textureName, int u, int v, int width, int height) {
		this(textureName, 256, 256, u, v, width, height);
	}

	public TextureBlitData(ResourceLocation textureName, int xOffset, int yOffset, int textureWidth, int textureHeight, int u, int v, int width, int height) {
		this.textureName = textureName;
		this.xOffset = xOffset;
		this.yOffset = yOffset;
		this.textureWidth = textureWidth;
		this.textureHeight = textureHeight;
		this.u = u;
		this.v = v;
		this.width = width;
		this.height = height;
	}

	public ResourceLocation getTextureName() {
		return textureName;
	}

	public int getWidth() {
		return width;
	}

	public int getTextureWidth() {
		return textureWidth;
	}

	public int getTextureHeight() {
		return textureHeight;
	}

	public int getU() {
		return u;
	}

	public int getV() {
		return v;
	}

	public int getHeight() {
		return height;
	}

	public int getXOffset() {
		return xOffset;
	}

	public int getYOffset() {
		return yOffset;
	}
}
