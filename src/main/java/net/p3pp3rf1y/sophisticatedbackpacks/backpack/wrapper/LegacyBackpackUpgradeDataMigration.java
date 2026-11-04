package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.RegistryAccess;
import net.minecraft.nbt.ByteTag;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.StringTag;
import net.minecraft.nbt.Tag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil.AnvilUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit.DepositUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InventoryOrder;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill.RefillUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock.RestockUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing.SmithingUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapMode;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper.ToolSwapperUpgradeItem;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.util.LegacyUpgradeDataMigration;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public final class LegacyBackpackUpgradeDataMigration {
	private LegacyBackpackUpgradeDataMigration() {
	}

	public static void registerMigrators() {
		LegacyUpgradeDataMigration.registerMigrator(ToolSwapperUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateToolSwapper);
		LegacyUpgradeDataMigration.registerMigrator(RefillUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateRefill);
		LegacyUpgradeDataMigration.registerMigrator(InceptionUpgradeItem.class,
				(upgradeStack, legacyData, registryAccess) -> migrateInception(upgradeStack, legacyData));
		LegacyUpgradeDataMigration.registerMigrator(AnvilUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateAnvil);
		LegacyUpgradeDataMigration.registerMigrator(SmithingUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateSmithing);
		LegacyUpgradeDataMigration.registerMigrator(DepositUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateDeposit);
		LegacyUpgradeDataMigration.registerMigrator(RestockUpgradeItem.class, LegacyBackpackUpgradeDataMigration::migrateRestock);
	}

	public static boolean migrateToolSwapper(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		boolean migrated = LegacyUpgradeDataMigration.migrateRootFilterAttributes(upgradeStack, legacyData, registryAccess);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "shouldSwapWeapon", ModDataComponents.SHOULD_SWAP_WEAPON.get(),
				LegacyBackpackUpgradeDataMigration::parseBoolean);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "toolSwapMode", ModDataComponents.TOOL_SWAP_MODE.get(),
				LegacyBackpackUpgradeDataMigration::parseToolSwapMode);
		return migrated;
	}

	public static boolean migrateRefill(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		boolean migrated = LegacyUpgradeDataMigration.migrateRootFilterAttributes(upgradeStack, legacyData, registryAccess, true);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "targetSlots", ModDataComponents.TARGET_SLOTS.get(),
				LegacyBackpackUpgradeDataMigration::parseTargetSlots);
		return migrated;
	}

	public static boolean migrateInception(ItemStack upgradeStack, CompoundTag legacyData) {
		return LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "inventoryOrder", ModDataComponents.INVENTORY_ORDER.get(),
				LegacyBackpackUpgradeDataMigration::parseInventoryOrder);
	}

	public static boolean migrateAnvil(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		boolean migrated = LegacyUpgradeDataMigration.migrateInventory(upgradeStack, legacyData, "craftingInventory", registryAccess);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "shiftClickIntoStorage",
				ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE.get(), LegacyBackpackUpgradeDataMigration::parseBoolean);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "itemName", ModDataComponents.ITEM_NAME.get(),
				LegacyBackpackUpgradeDataMigration::parseString);
		return migrated;
	}

	public static boolean migrateSmithing(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		boolean migrated = LegacyUpgradeDataMigration.migrateInventory(upgradeStack, legacyData, "craftingInventory", registryAccess);
		migrated |= LegacyUpgradeDataMigration.migrateComponent(upgradeStack, legacyData, "shiftClickIntoStorage",
				ModCoreDataComponents.SHIFT_CLICK_INTO_STORAGE.get(), LegacyBackpackUpgradeDataMigration::parseBoolean);
		return migrated;
	}

	public static boolean migrateDeposit(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		CompoundTag originalLegacyData = legacyData.copy();
		Tag filterByInventory = legacyData.get("filterByInventory");
		boolean filterAttributesPresent = upgradeStack.has(ModCoreDataComponents.FILTER_ATTRIBUTES.get());
		boolean moduleComponentPresent = upgradeStack.has(ModDataComponents.FILTER_BY_INVENTORY.get());
		LegacyUpgradeDataMigration.migrateRootFilterAttributes(upgradeStack, legacyData, registryAccess);
		Optional<Boolean> parsedFilterByInventory = filterByInventory == null ? Optional.empty() : parseBoolean(filterByInventory);
		boolean moduleComponentSet = false;
		if (filterByInventory != null && !moduleComponentPresent && parsedFilterByInventory.isPresent()) {
			upgradeStack.set(ModDataComponents.FILTER_BY_INVENTORY.get(), parsedFilterByInventory.get());
			moduleComponentSet = true;
		}
		if (filterByInventory != null
				&& (!upgradeStack.has(ModCoreDataComponents.FILTER_ATTRIBUTES.get()) || !moduleComponentPresent && parsedFilterByInventory.isEmpty())) {
			legacyData.put("filterByInventory", filterByInventory.copy());
		}
		return !filterAttributesPresent && upgradeStack.has(ModCoreDataComponents.FILTER_ATTRIBUTES.get()) || moduleComponentSet
				|| !originalLegacyData.equals(legacyData);
	}

	public static boolean migrateRestock(ItemStack upgradeStack, CompoundTag legacyData, RegistryAccess registryAccess) {
		return LegacyUpgradeDataMigration.migrateRootFilterAttributes(upgradeStack, legacyData, registryAccess);
	}

	private static Optional<Boolean> parseBoolean(Tag tag) {
		if (tag.getId() != Tag.TAG_BYTE) {
			return Optional.empty();
		}
		byte value = ((ByteTag) tag).getAsByte();
		return value == 0 || value == 1 ? Optional.of(value == 1) : Optional.empty();
	}

	private static Optional<String> parseString(Tag tag) {
		return tag instanceof StringTag stringTag ? Optional.of(stringTag.getAsString()) : Optional.empty();
	}

	private static Optional<ToolSwapMode> parseToolSwapMode(Tag tag) {
		if (!(tag instanceof StringTag stringTag)) {
			return Optional.empty();
		}
		return ToolSwapMode.CODEC.parse(NbtOps.INSTANCE, stringTag).result();
	}

	private static Optional<InventoryOrder> parseInventoryOrder(Tag tag) {
		if (!(tag instanceof StringTag stringTag)) {
			return Optional.empty();
		}
		return InventoryOrder.CODEC.parse(NbtOps.INSTANCE, stringTag).result();
	}

	private static Optional<Map<Integer, RefillUpgradeWrapper.TargetSlot>> parseTargetSlots(Tag tag) {
		if (!(tag instanceof CompoundTag targetSlotsTag)) {
			return Optional.empty();
		}
		Map<Integer, RefillUpgradeWrapper.TargetSlot> targetSlots = new HashMap<>();
		for (String key : targetSlotsTag.getAllKeys()) {
			int slot;
			try {
				slot = Integer.parseInt(key);
			} catch (NumberFormatException exception) {
				return Optional.empty();
			}
			Tag value = targetSlotsTag.get(key);
			if (!(value instanceof StringTag stringTag)) {
				return Optional.empty();
			}
			Optional<RefillUpgradeWrapper.TargetSlot> targetSlot = parseTargetSlot(stringTag.getAsString());
			if (targetSlot.isEmpty()) {
				return Optional.empty();
			}
			targetSlots.put(slot, targetSlot.get());
		}
		return Optional.of(Map.copyOf(targetSlots));
	}

	private static Optional<RefillUpgradeWrapper.TargetSlot> parseTargetSlot(String name) {
		for (RefillUpgradeWrapper.TargetSlot targetSlot : RefillUpgradeWrapper.TargetSlot.values()) {
			if (targetSlot.getSerializedName().equals(name)) {
				return Optional.of(targetSlot);
			}
		}
		return Optional.empty();
	}
}
