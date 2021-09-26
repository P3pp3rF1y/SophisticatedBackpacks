package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.util.GsonHelper;

import java.util.Optional;
import java.util.function.Predicate;

abstract class TypedMatcherFactory<T> implements IMatcherFactory<T> {
	private final String typeName;

	protected TypedMatcherFactory(String typeName) {
		this.typeName = typeName;
	}

	@Override
	public boolean appliesTo(JsonElement jsonElement) {
		return jsonElement.isJsonObject() && GsonHelper.getAsString(jsonElement.getAsJsonObject(), "type").equals(typeName);
	}

	@Override
	public Optional<Predicate<T>> getPredicate(JsonElement jsonElement) {
		return getPredicateFromObject(jsonElement.getAsJsonObject());
	}

	protected abstract Optional<Predicate<T>> getPredicateFromObject(JsonObject jsonObject);
}
