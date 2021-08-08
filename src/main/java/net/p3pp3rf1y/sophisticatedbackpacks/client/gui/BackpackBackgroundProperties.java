package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.util.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;

public class BackpackBackgroundProperties {
	public static final BackpackBackgroundProperties REGULAR = new BackpackBackgroundProperties(9, 0,
			RegistryHelper.getRL("textures/gui/backpack_background_9.png"));
	public static final BackpackBackgroundProperties WIDE = new BackpackBackgroundProperties(12, 27,
			RegistryHelper.getRL("textures/gui/backpack_background_12.png"));

	private final int slotsOnLine;
	private final int playerInventoryXOffset;
	private final ResourceLocation textureName;

	private BackpackBackgroundProperties(int slotsOnLine, int playerInventoryXOffset, ResourceLocation textureName) {

		this.slotsOnLine = slotsOnLine;
		this.playerInventoryXOffset = playerInventoryXOffset;
		this.textureName = textureName;
	}

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
