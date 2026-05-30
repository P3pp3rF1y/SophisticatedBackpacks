package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;

import java.util.UUID;

public record CapturedMob(UUID id, ResourceLocation entityType, CompoundTag entityNbt, int slot, int width, int height, int slotCost, boolean hostile, String displayName,
		int currentHealth, int maxHealth) {
	public boolean occupiesSlot(int inventorySlot, int columns) {
		int left = slot % columns;
		int top = slot / columns;
		int x = inventorySlot % columns;
		int y = inventorySlot / columns;
		return x >= left && x < left + width && y >= top && y < top + height;
	}
}
