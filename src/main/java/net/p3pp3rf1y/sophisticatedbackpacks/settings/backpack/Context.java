package net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack;

public enum Context {
	PLAYER(0),
	BACKPACK(1);

	private final int id;

	Context(int id) {
		this.id = id;
	}

	public int getId() {
		return id;
	}

	public static Context fromId(int id) {
		if (PLAYER.id == id) {
			return PLAYER;
		} else {
			return BACKPACK;
		}
	}
}
