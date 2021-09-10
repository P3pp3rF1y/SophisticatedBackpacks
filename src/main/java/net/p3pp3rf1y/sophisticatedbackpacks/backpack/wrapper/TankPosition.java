package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.util.IStringSerializable;

import java.util.Locale;

public enum TankPosition implements IStringSerializable {
	LEFT,
	RIGHT;

	@Override
	public String getSerializedName() {
		return name().toLowerCase(Locale.ENGLISH);
	}
}
