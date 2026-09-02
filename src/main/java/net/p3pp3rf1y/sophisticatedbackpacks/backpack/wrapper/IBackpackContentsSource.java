package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;

import java.util.Optional;
import java.util.UUID;

interface IBackpackContentsSource {
	CompoundTag getContents();

	void setContents(CompoundTag contents);

	void markDirty();

	Optional<UUID> getContentsUuid();

	default boolean usesCanonicalSlotNumbers() {
		return false;
	}
}
