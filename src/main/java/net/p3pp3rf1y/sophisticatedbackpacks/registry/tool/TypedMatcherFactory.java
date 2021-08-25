package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.util.JSONUtils;

import java.util.Optional;
import java.util.function.Predicate;

abstract class TypedMatcherFactory<T> implements IMatcherFactory<T> {
	private final String typeName;

	public TypedMatcherFactory(String typeName) {
		this.typeName = typeName;
	}

	@Override
	public boolean appliesTo(JsonElement jsonElement) {
		return jsonElement.isJsonObject() && JSONUtils.getAsString(jsonElement.getAsJsonObject(), "type").equals(typeName);
	}

	@Override
	public Optional<Predicate<T>> getPredicate(JsonElement jsonElement) {
		return getPredicateFromObject(jsonElement.getAsJsonObject());
	}

	protected abstract Optional<Predicate<T>> getPredicateFromObject(JsonObject jsonObject);
}
