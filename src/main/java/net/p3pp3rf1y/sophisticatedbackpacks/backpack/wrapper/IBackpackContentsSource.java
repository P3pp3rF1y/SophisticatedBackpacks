package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

import java.util.Optional;
import java.util.UUID;

interface IBackpackContentsSource {
	ContainerContents getContents();

	void setContents(ContainerContents contents);

	void markDirty();

	Optional<UUID> getContentsUuid();

	default boolean usesLegacyBackpackDataMigration() {
		return false;
	}

	default boolean usesCanonicalSlotNumbers() {
		return false;
	}
}
