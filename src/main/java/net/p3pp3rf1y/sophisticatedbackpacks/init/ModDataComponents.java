package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;

import javax.annotation.Nullable;

import java.util.UUID;

public class ModDataComponents {
	public static final String TIER_UPGRADE_SOURCE_COLUMNS = "tier_upgrade_source_columns";
	public static final String TIER_UPGRADE_SOURCE_INVENTORY_SLOTS = "tier_upgrade_source_inventory_slots";
	public static final String TIER_UPGRADE_SOURCE_UPGRADE_SLOTS = "tier_upgrade_source_upgrade_slots";
	public static final String TIER_UPGRADE_OPERATION_ID = "tier_upgrade_operation_id";

	private ModDataComponents() {
	}

	@Nullable
	public static Integer getSourceColumns(ItemStack stack) {
		CompoundTag tag = stack.getTag();
		return tag != null && tag.contains(TIER_UPGRADE_SOURCE_COLUMNS) ? tag.getInt(TIER_UPGRADE_SOURCE_COLUMNS) : null;
	}

	@Nullable
	public static Integer getSourceInventorySlots(ItemStack stack) {
		CompoundTag tag = stack.getTag();
		return tag != null && tag.contains(TIER_UPGRADE_SOURCE_INVENTORY_SLOTS) ? tag.getInt(TIER_UPGRADE_SOURCE_INVENTORY_SLOTS) : null;
	}

	@Nullable
	public static Integer getSourceUpgradeSlots(ItemStack stack) {
		CompoundTag tag = stack.getTag();
		return tag != null && tag.contains(TIER_UPGRADE_SOURCE_UPGRADE_SLOTS) ? tag.getInt(TIER_UPGRADE_SOURCE_UPGRADE_SLOTS) : null;
	}

	@Nullable
	public static UUID getOperationId(ItemStack stack) {
		CompoundTag tag = stack.getTag();
		return tag != null && tag.hasUUID(TIER_UPGRADE_OPERATION_ID) ? tag.getUUID(TIER_UPGRADE_OPERATION_ID) : null;
	}

	public static void setSourceColumns(ItemStack stack, int columns) {
		stack.getOrCreateTag().putInt(TIER_UPGRADE_SOURCE_COLUMNS, columns);
	}

	public static void setSourceInventorySlots(ItemStack stack, int slots) {
		stack.getOrCreateTag().putInt(TIER_UPGRADE_SOURCE_INVENTORY_SLOTS, slots);
	}

	public static void setSourceUpgradeSlots(ItemStack stack, int slots) {
		stack.getOrCreateTag().putInt(TIER_UPGRADE_SOURCE_UPGRADE_SLOTS, slots);
	}

	public static void setOperationId(ItemStack stack, UUID operationId) {
		stack.getOrCreateTag().putUUID(TIER_UPGRADE_OPERATION_ID, operationId);
	}

	public static void clearTierUpgrade(ItemStack stack) {
		stack.removeTagKey(TIER_UPGRADE_SOURCE_COLUMNS);
		stack.removeTagKey(TIER_UPGRADE_SOURCE_INVENTORY_SLOTS);
		stack.removeTagKey(TIER_UPGRADE_SOURCE_UPGRADE_SLOTS);
		stack.removeTagKey(TIER_UPGRADE_OPERATION_ID);
	}
}
