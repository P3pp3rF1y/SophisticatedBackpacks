package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

public record CapturedMobFootprint(int width, int height) {
	public int area() {
		return width * height;
	}
}
