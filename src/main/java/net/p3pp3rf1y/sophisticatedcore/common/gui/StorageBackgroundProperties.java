package net.p3pp3rf1y.sophisticatedcore.common.gui;

import net.minecraft.resources.ResourceLocation;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;

public record StorageBackgroundProperties(int slotsOnLine, int playerInventoryXOffset, ResourceLocation textureName) {
	public static final StorageBackgroundProperties REGULAR = new StorageBackgroundProperties(9, 0,
			SophisticatedCore.getRL("textures/gui/storage_background_9.png"));
	public static final StorageBackgroundProperties WIDE = new StorageBackgroundProperties(12, 27,
			SophisticatedCore.getRL("textures/gui/storage_background_12.png"));

	public int getSlotsOnLine() {
		return slotsOnLine;
	}

	public int getPlayerInventoryXOffset() {
		return playerInventoryXOffset;
	}

	public ResourceLocation getTextureName() {
		return textureName;
	}
}
