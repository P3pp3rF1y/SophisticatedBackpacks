package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

class ClientLinkedStorageBackpackContentsTest {
	@Test
	void installRetainsNewestCanonicalContentsAndLayout() {
		UUID groupId = UUID.randomUUID();
		CompoundTag initial = new CompoundTag();
		initial.putString("state", "initial");
		CompoundTag updated = new CompoundTag();
		updated.putString("state", "updated");

		ClientLinkedStorageBackpackContents.install(groupId, 1, initial, Component.literal("Backpack"), 27, 3, 1);
		ClientLinkedStorageBackpackContents.install(groupId, 2, updated, Component.literal("Backpack"), 54, 5, 2);
		ClientLinkedStorageBackpackContents.install(groupId, 1, initial, Component.literal("Old"), 9, 1, 0);

		assertEquals("updated", ClientLinkedStorageBackpackContents.getBinding(groupId).orElseThrow().getContents().getString("state"));
		assertEquals(2, ClientLinkedStorageBackpackContents.getBinding(groupId).orElseThrow().getColumnsTaken());
		ClientLinkedStorageBackpackContents.clear();
	}
}
