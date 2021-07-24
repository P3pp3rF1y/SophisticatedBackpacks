package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.util.IStringSerializable;

public enum TankPosition implements IStringSerializable {
	LEFT,
	RIGHT;

	@Override
	public String getString() {
		return name().toLowerCase();
	}
}
