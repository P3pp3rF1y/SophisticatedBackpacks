package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.JsonObject;

import javax.annotation.Nullable;

public interface IRegistryDataLoader {
	String getName();

	void parse(JsonObject json, @Nullable String modId);

	void clear();
}
