package net.p3pp3rf1y.sophisticatedcore.settings;

import net.minecraft.nbt.CompoundTag;
import org.apache.logging.log4j.util.TriConsumer;

import java.util.Optional;
import java.util.function.BiFunction;

public class GlobalOverridableSetting<T> {
	private final String tagName;
	private final BiFunction<CompoundTag, String, Optional<T>> getValue;
	private final TriConsumer<CompoundTag, String, T> setValue;
	private final T defaultValue;

	public GlobalOverridableSetting(String tagName, BiFunction<CompoundTag, String, Optional<T>> getValue, TriConsumer<CompoundTag, String, T> setValue, T defaultValue) {
		this.tagName = tagName;
		this.getValue = getValue;
		this.setValue = setValue;
		this.defaultValue = defaultValue;
	}

	public String getName() {
		return tagName;
	}

	public void setValue(CompoundTag tag, T value) {
		setValue.accept(tag, tagName, value);
	}

	public void removeFrom(CompoundTag tag) {
		tag.remove(tagName);
	}

	public Optional<T> getValue(CompoundTag tag) {
		return getValue.apply(tag, tagName);
	}

	public T getDefaultValue() {
		return defaultValue;
	}
}
