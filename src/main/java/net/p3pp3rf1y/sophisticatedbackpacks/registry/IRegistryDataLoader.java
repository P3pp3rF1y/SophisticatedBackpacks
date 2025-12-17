package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.JsonObject;
import org.jspecify.annotations.Nullable;

public interface IRegistryDataLoader {
	String getName();

	void parse(JsonObject json, @Nullable String modId);

	void clear();
}
