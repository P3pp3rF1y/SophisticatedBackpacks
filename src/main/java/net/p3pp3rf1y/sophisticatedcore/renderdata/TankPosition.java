package net.p3pp3rf1y.sophisticatedcore.renderdata;

import net.minecraft.util.StringRepresentable;

import java.util.Locale;

public enum TankPosition implements StringRepresentable {
	LEFT,
	RIGHT;

	@Override
	public String getSerializedName() {
		return name().toLowerCase(Locale.ENGLISH);
	}
}
