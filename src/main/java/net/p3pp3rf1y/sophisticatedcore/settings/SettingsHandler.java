package net.p3pp3rf1y.sophisticatedcore.settings;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.settings.globaloverridable.GlobalOverridableSettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.nosort.NoSortSettingsCategory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Supplier;

public abstract class SettingsHandler {
	protected CompoundTag contentsNbt;
	protected final Runnable markContentsDirty;
	protected final Map<String, ISettingsCategory> settingsCategories = new LinkedHashMap<>();
	private final Map<Class<?>, List<?>> interfaceCategories = new HashMap<>();
	private final Map<Class<? extends ISettingsCategory>, ISettingsCategory> typeCategories = new HashMap<>();

	public SettingsHandler(CompoundTag contentsNbt, Runnable markContentsDirty, String playerSettingsTagName, Supplier<InventoryHandler> inventoryHandlerSupplier, Supplier<RenderInfo> renderInfoSupplier) {
		this.contentsNbt = contentsNbt;
		this.markContentsDirty = markContentsDirty;
		addSettingsCategories(inventoryHandlerSupplier, renderInfoSupplier, getSettingsNbtFromContentsNbt(contentsNbt), playerSettingsTagName);
	}

	protected abstract CompoundTag getSettingsNbtFromContentsNbt(CompoundTag contentsNbt);

	private void addSettingsCategories(Supplier<InventoryHandler> inventoryHandlerSupplier, Supplier<RenderInfo> renderInfoSupplier, CompoundTag settingsNbt, String playerSettingsTagName) {
		addSettingsCategory(settingsNbt, GlobalOverridableSettingsCategory.NAME, markContentsDirty, (categoryNbt, saveNbt) -> new GlobalOverridableSettingsCategory(categoryNbt, saveNbt, playerSettingsTagName));
		addSettingsCategory(settingsNbt, NoSortSettingsCategory.NAME, markContentsDirty, NoSortSettingsCategory::new);
		addSettingsCategory(settingsNbt, MemorySettingsCategory.NAME, markContentsDirty, (categoryNbt, saveNbt) -> new MemorySettingsCategory(inventoryHandlerSupplier, categoryNbt, saveNbt));
		addSettingsCategory(settingsNbt, ItemDisplaySettingsCategory.NAME, markContentsDirty, (categoryNbt, saveNbt) -> new ItemDisplaySettingsCategory(inventoryHandlerSupplier, renderInfoSupplier, categoryNbt, saveNbt));
	}

	private void addSettingsCategory(CompoundTag settingsNbt, String categoryName, Runnable markContentsDirty, BiFunction<CompoundTag, Consumer<CompoundTag>, ISettingsCategory> instantiateCategory) {
		ISettingsCategory category = instantiateCategory.apply(settingsNbt.getCompound(categoryName), tag -> {
			saveCategoryNbt(settingsNbt, categoryName, tag);
			markContentsDirty.run();
		});
		settingsCategories.put(categoryName, category);
		typeCategories.put(category.getClass(), category);
	}

	protected abstract void saveCategoryNbt(CompoundTag settingsNbt, String categoryName, CompoundTag tag);

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

	public CompoundTag getNbt() {
		return getSettingsNbtFromContentsNbt(contentsNbt);
	}

	public void reloadFrom(CompoundTag contentsNbt) {
		CompoundTag settingsNbt = getSettingsNbtFromContentsNbt(contentsNbt);
		getSettingsCategories().forEach((categoryName, category) -> category.reloadFrom(settingsNbt.getCompound(categoryName)));
	}
}
