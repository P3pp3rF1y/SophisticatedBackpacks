package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import net.minecraft.util.JSONUtils;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

public class JsonHelper {
	private JsonHelper() {}

	public static <V> Set<V> setFromJson(JsonElement element, Function<JsonElement, V> getElement) {
		return setFromJson(JSONUtils.convertToJsonArray(element, ""), getElement);
	}

	private static <V> Set<V> setFromJson(JsonArray array, Function<JsonElement, V> getElement) {
		Set<V> ret = new HashSet<>();

		for (JsonElement element : array) {
			ret.add(getElement.apply(element));
		}

		return ret;
	}
}
