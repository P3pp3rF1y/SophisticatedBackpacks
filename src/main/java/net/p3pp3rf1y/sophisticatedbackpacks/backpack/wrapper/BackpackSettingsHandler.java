package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
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
	private final CompoundNBT backpackContentsNbt;
	private final Runnable markBackpackContentsDirty;
	private final Map<Class<?>, List<?>> interfaceCategories = new HashMap<>();
	private final Map<String, ISettingsCategory> settingsCategories = new LinkedHashMap<>();
	private final Map<Class<? extends ISettingsCategory>, ISettingsCategory> typeCategories = new HashMap<>();

	public BackpackSettingsHandler(CompoundNBT backpackContentsNbt, Runnable markBackpackContentsDirty) {
		this.backpackContentsNbt = backpackContentsNbt;
		this.markBackpackContentsDirty = markBackpackContentsDirty;
		addSettingsCategories(backpackContentsNbt.getCompound(SETTINGS_TAG));
	}

	private void addSettingsCategories(CompoundNBT settingsNbt) {
		addSettingsCategory(settingsNbt, BackpackSettingsCategory.NAME, markBackpackContentsDirty, BackpackSettingsCategory::new);
		addSettingsCategory(settingsNbt, NoSortSettingsCategory.NAME, markBackpackContentsDirty, NoSortSettingsCategory::new);
	}

	private void addSettingsCategory(CompoundNBT settingsNbt, String categoryName, Runnable markBackpackContentsDirty, BiFunction<CompoundNBT, Consumer<CompoundNBT>, ISettingsCategory> instantiateCategory) {
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

	public CompoundNBT getNbt() {
		return backpackContentsNbt.getCompound(SETTINGS_TAG);
	}

	public void reloadFrom(CompoundNBT backpackContentsNbt) {
		CompoundNBT settingsNbt = backpackContentsNbt.getCompound(SETTINGS_TAG);
		settingsCategories.forEach((categoryName, category) -> category.reloadFrom(settingsNbt.getCompound(categoryName)));
	}
}
