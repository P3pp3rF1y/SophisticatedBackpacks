package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsCategory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Consumer;

public class BackpackSettingsHandler {
	public static final String SETTINGS_TAG = "settings";
	private final CompoundTag backpackContentsNbt;
	private final Runnable markBackpackContentsDirty;
	private final Map<Class<?>, List<?>> interfaceCategories = new HashMap<>();
	private final Map<String, ISettingsCategory> settingsCategories = new LinkedHashMap<>();
	private final Map<Class<? extends ISettingsCategory>, ISettingsCategory> typeCategories = new HashMap<>();

	public BackpackSettingsHandler(IBackpackWrapper backpackWrapper, CompoundTag backpackContentsNbt, Runnable markBackpackContentsDirty) {
		this.backpackContentsNbt = backpackContentsNbt;
		this.markBackpackContentsDirty = markBackpackContentsDirty;
		addSettingsCategories(backpackWrapper, backpackContentsNbt.getCompound(SETTINGS_TAG));
	}

	private void addSettingsCategories(IBackpackWrapper backpackWrapper, CompoundTag settingsNbt) {
		addSettingsCategory(settingsNbt, BackpackSettingsCategory.NAME, markBackpackContentsDirty, BackpackSettingsCategory::new);
		addSettingsCategory(settingsNbt, NoSortSettingsCategory.NAME, markBackpackContentsDirty, NoSortSettingsCategory::new);
		addSettingsCategory(settingsNbt, MemorySettingsCategory.NAME, markBackpackContentsDirty, (categoryNbt, saveNbt) -> new MemorySettingsCategory(backpackWrapper, categoryNbt, saveNbt));
		addSettingsCategory(settingsNbt, ItemDisplaySettingsCategory.NAME, markBackpackContentsDirty, (categoryNbt, saveNbt) -> new ItemDisplaySettingsCategory(backpackWrapper, categoryNbt, saveNbt));
	}

	private void addSettingsCategory(CompoundTag settingsNbt, String categoryName, Runnable markBackpackContentsDirty, BiFunction<CompoundTag, Consumer<CompoundTag>, ISettingsCategory> instantiateCategory) {
		ISettingsCategory category = instantiateCategory.apply(settingsNbt.getCompound(categoryName), tag -> {
			settingsNbt.put(categoryName, tag);
			backpackContentsNbt.put(SETTINGS_TAG, settingsNbt);
			markBackpackContentsDirty.run();
		});
		settingsCategories.put(categoryName, category);
		typeCategories.put(category.getClass(), category);
	}

	public Map<String, ISettingsCategory> getSettingsCategories() {
		return settingsCategories;
	}

	public <T> List<T> getCategoriesThatImplement(Class<T> categoryClass) {
		//noinspection unchecked
		return (List<T>) interfaceCategories.computeIfAbsent(categoryClass, this::getListOfWrappersThatImplement);
	}

	public <T extends ISettingsCategory> T getTypeCategory(Class<T> categoryClazz) {
		//noinspection unchecked - only inserted in one place where it's made sure that class is the same as the category instance
		return (T) typeCategories.get(categoryClazz);
	}

	private <T> List<T> getListOfWrappersThatImplement(Class<T> uc) {
		List<T> ret = new ArrayList<>();
		for (ISettingsCategory category : settingsCategories.values()) {
			if (uc.isInstance(category)) {
				//noinspection unchecked
				ret.add((T) category);
			}
		}
		return ret;
	}

	public void copyTo(BackpackSettingsHandler settingsHandler) {
		if (backpackContentsNbt.contains(SETTINGS_TAG)) {
			//noinspection ConstantConditions - checking for whether tag exists just one line up
			settingsHandler.backpackContentsNbt.put(SETTINGS_TAG, backpackContentsNbt.get(SETTINGS_TAG));
		}
	}

	public CompoundTag getNbt() {
		return backpackContentsNbt.getCompound(SETTINGS_TAG);
	}

	public void reloadFrom(CompoundTag backpackContentsNbt) {
		CompoundTag settingsNbt = backpackContentsNbt.getCompound(SETTINGS_TAG);
		settingsCategories.forEach((categoryName, category) -> category.reloadFrom(settingsNbt.getCompound(categoryName)));
	}
}
