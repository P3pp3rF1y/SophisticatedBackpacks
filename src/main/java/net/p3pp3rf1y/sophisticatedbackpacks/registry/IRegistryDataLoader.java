package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.JsonObject;

public interface IRegistryDataLoader {
	String getName();

	void parse(JsonObject json);

	void clear();
}
