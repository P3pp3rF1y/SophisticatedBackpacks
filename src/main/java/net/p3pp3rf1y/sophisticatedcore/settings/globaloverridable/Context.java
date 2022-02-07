package net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable;

public enum Context {
	PLAYER(0),
	STORAGE(1);

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
			return STORAGE;
		}
	}
}
