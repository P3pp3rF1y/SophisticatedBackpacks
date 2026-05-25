package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.component.DataComponents;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtUtils;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;

import java.util.Optional;
import java.util.UUID;

public final class LegacyBackpackDataMigration {
	private static final String CLOTH_COLOR_TAG = "clothColor";
	private static final String BORDER_COLOR_TAG = "borderColor";
	private static final String OPEN_TAB_ID_TAG = "openTabId";
	private static final String SORT_BY_TAG = "sortBy";
	private static final String CONTENTS_UUID_TAG = "contentsUuid";
	private static final String INVENTORY_SLOTS_TAG = "inventorySlots";
	private static final String UPGRADE_SLOTS_TAG = "upgradeSlots";
	private static final String LOOT_TABLE_NAME_TAG = "lootTableName";
	private static final String LOOT_PERCENTAGE_TAG = "lootPercentage";
	private static final String COLUMNS_TAKEN_TAG = "columnsTaken";
	private static final String TEMPLATE_NAME_TAG = "templateName";
	private static final String RENDER_INFO_TAG = "renderInfo";
	private static final String REAL_COUNT_TAG = "realCount";

	private LegacyBackpackDataMigration() {}

	public static void normalizeLegacyData(ItemStack backpack) {
		if (!backpack.has(ModCoreDataComponents.STORAGE_UUID)) {
			getContentsUuid(backpack).ifPresent(uuid -> backpack.set(ModCoreDataComponents.STORAGE_UUID, uuid));
		}
		if (!backpack.has(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS)) {
			getInventorySlots(backpack).ifPresent(slots -> backpack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, slots));
		}
		if (!backpack.has(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS)) {
			getUpgradeSlots(backpack).ifPresent(slots -> backpack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, slots));
		}
		if (!backpack.has(ModCoreDataComponents.MAIN_COLOR)) {
			getMainColor(backpack).ifPresent(color -> backpack.set(ModCoreDataComponents.MAIN_COLOR, color));
		}
		if (!backpack.has(ModCoreDataComponents.ACCENT_COLOR)) {
			getAccentColor(backpack).ifPresent(color -> backpack.set(ModCoreDataComponents.ACCENT_COLOR, color));
		}
		if (!backpack.has(ModCoreDataComponents.OPEN_TAB_ID)) {
			getOpenTabId(backpack).ifPresent(openTabId -> backpack.set(ModCoreDataComponents.OPEN_TAB_ID, openTabId));
		}
		if (!backpack.has(ModCoreDataComponents.SORT_BY)) {
			getSortBy(backpack).ifPresent(sortBy -> backpack.set(ModCoreDataComponents.SORT_BY, sortBy));
		}
		if (!backpack.has(ModDataComponents.COLUMNS_TAKEN)) {
			getColumnsTaken(backpack).ifPresent(columnsTaken -> backpack.set(ModDataComponents.COLUMNS_TAKEN, columnsTaken));
		}
		if (!backpack.has(ModDataComponents.LOOT_TABLE)) {
			getLootTableName(backpack).ifPresent(lootTable -> backpack.set(ModDataComponents.LOOT_TABLE, lootTable));
		}
		if (!backpack.has(ModDataComponents.LOOT_FACTOR)) {
			getLootPercentage(backpack).ifPresent(lootPercentage -> backpack.set(ModDataComponents.LOOT_FACTOR, lootPercentage));
		}
		if (!backpack.has(ModDataComponents.TEMPLATE_NAME)) {
			getTemplateName(backpack).ifPresent(templateName -> backpack.set(ModDataComponents.TEMPLATE_NAME, templateName));
		}
	}

	public static void migrateBackpackContents(ItemStack backpack, UUID newUuid) {
		migrateNbtTag(backpack, newUuid, InventoryHandler.INVENTORY_TAG);
		migrateNbtTag(backpack, newUuid, UpgradeHandler.UPGRADE_INVENTORY_TAG);
	}

	public static void normalizeLegacyBackpackContents(CompoundTag contentsNbt) {
		normalizeInventory(contentsNbt, InventoryHandler.INVENTORY_TAG);
		normalizeInventory(contentsNbt, UpgradeHandler.UPGRADE_INVENTORY_TAG);
	}

	private static void normalizeInventory(CompoundTag contentsNbt, String inventoryTag) {
		if (!contentsNbt.contains(inventoryTag)) {
			return;
		}

		CompoundTag inventoryNbt = contentsNbt.getCompound(inventoryTag);
		if (!inventoryNbt.contains("Items")) {
			return;
		}

		ListTag items = inventoryNbt.getList("Items", Tag.TAG_COMPOUND);
		for (Tag item : items) {
			normalizeItemStackCount((CompoundTag) item);
		}
	}

	private static void normalizeItemStackCount(CompoundTag itemTag) {
		if (itemTag.contains("count")) {
			return;
		}

		if (itemTag.contains(REAL_COUNT_TAG)) {
			itemTag.putInt("count", itemTag.getInt(REAL_COUNT_TAG));
		} else if (itemTag.contains("Count")) {
			itemTag.putInt("count", itemTag.getByte("Count"));
		}
	}

	private static void migrateNbtTag(ItemStack backpack, UUID newUuid, String key) {
		getCompound(backpack, key).ifPresent(nbt -> {
			normalizeInventoryNbt(nbt);
			BackpackStorage.get().getOrCreateBackpackContents(newUuid).put(key, nbt);
			BackpackStorage.get().setDirty();
			CustomData.update(DataComponents.CUSTOM_DATA, backpack, customData -> customData.remove(key));
		});
	}

	private static void normalizeInventoryNbt(CompoundTag inventoryNbt) {
		if (!inventoryNbt.contains("Items")) {
			return;
		}

		ListTag items = inventoryNbt.getList("Items", Tag.TAG_COMPOUND);
		for (Tag item : items) {
			normalizeItemStackCount((CompoundTag) item);
		}
	}

	public static Optional<UUID> getContentsUuid(ItemStack backpack) {
		return getLegacyCustomData(backpack).flatMap(tag -> {
			Tag uuidTag = tag.get(CONTENTS_UUID_TAG);
			if (uuidTag == null) {
				return Optional.empty();
			}
			return Optional.of(NbtUtils.loadUUID(uuidTag));
		});
	}

	public static Optional<Integer> getInventorySlots(ItemStack backpack) {
		return getInt(backpack, INVENTORY_SLOTS_TAG);
	}

	public static Optional<Integer> getUpgradeSlots(ItemStack backpack) {
		return getInt(backpack, UPGRADE_SLOTS_TAG);
	}

	public static Optional<Integer> getMainColor(ItemStack backpack) {
		return getInt(backpack, CLOTH_COLOR_TAG).map(LegacyBackpackDataMigration::withOpaqueAlpha);
	}

	public static Optional<Integer> getAccentColor(ItemStack backpack) {
		return getInt(backpack, BORDER_COLOR_TAG).map(LegacyBackpackDataMigration::withOpaqueAlpha);
	}

	public static int withOpaqueAlpha(int color) {
		return (color & 0xFF_000000) == 0 ? color | 0xFF_000000 : color;
	}

	public static Optional<Integer> getOpenTabId(ItemStack backpack) {
		return getInt(backpack, OPEN_TAB_ID_TAG);
	}

	public static Optional<SortBy> getSortBy(ItemStack backpack) {
		return getString(backpack, SORT_BY_TAG).map(SortBy::fromName);
	}

	public static Optional<Integer> getColumnsTaken(ItemStack backpack) {
		return getInt(backpack, COLUMNS_TAKEN_TAG);
	}

	public static Optional<ResourceLocation> getLootTableName(ItemStack backpack) {
		return getString(backpack, LOOT_TABLE_NAME_TAG).map(ResourceLocation::parse);
	}

	public static Optional<Float> getLootPercentage(ItemStack backpack) {
		return getLegacyCustomData(backpack).flatMap(tag -> tag.contains(LOOT_PERCENTAGE_TAG) ? Optional.of(tag.getFloat(LOOT_PERCENTAGE_TAG)) : Optional.empty());
	}

	public static Optional<ResourceLocation> getTemplateName(ItemStack backpack) {
		return getString(backpack, TEMPLATE_NAME_TAG).map(ResourceLocation::parse);
	}

	public static Optional<CompoundTag> getRenderInfo(ItemStack backpack) {
		return getCompound(backpack, RENDER_INFO_TAG);
	}

	private static Optional<Integer> getInt(ItemStack backpack, String key) {
		return getLegacyCustomData(backpack).flatMap(tag -> tag.contains(key) ? Optional.of(tag.getInt(key)) : Optional.empty());
	}

	private static Optional<String> getString(ItemStack backpack, String key) {
		return getLegacyCustomData(backpack).flatMap(tag -> tag.contains(key) ? Optional.of(tag.getString(key)) : Optional.empty());
	}

	private static Optional<CompoundTag> getCompound(ItemStack backpack, String key) {
		return getLegacyCustomData(backpack).flatMap(tag -> tag.contains(key) ? Optional.of(tag.getCompound(key)) : Optional.empty());
	}

	private static Optional<CompoundTag> getLegacyCustomData(ItemStack backpack) {
		return Optional.ofNullable(backpack.get(DataComponents.CUSTOM_DATA)).map(CustomData::copyTag);
	}
}
