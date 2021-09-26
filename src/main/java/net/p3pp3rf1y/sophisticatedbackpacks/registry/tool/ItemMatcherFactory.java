package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.util.GsonHelper;

import java.util.Optional;

abstract class ItemMatcherFactory {
	private final String typeName;

	public ItemMatcherFactory(String typeName) {
		this.typeName = typeName;
	}

	public boolean appliesTo(JsonElement jsonElement) {
		return jsonElement.isJsonObject() && GsonHelper.getAsString(jsonElement.getAsJsonObject(), "type").equals(typeName);
	}

	public Optional<ItemTagMatcher> getPredicate(JsonElement jsonElement) {
		return getPredicateFromObject(jsonElement.getAsJsonObject());
	}

	protected abstract Optional<ItemTagMatcher> getPredicateFromObject(JsonObject jsonObject);
}
