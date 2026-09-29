package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.SharedConstants;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import sun.misc.Unsafe;

import java.lang.reflect.Field;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackBlockEntityTest {
	private static BackpackItem testBackpack;

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		testBackpack = registerBackpack("backpack_block_entity_persistence_test", 36, 4);
	}

	@Test
	void loadPreservesPendingBackpackAndTints() throws ReflectiveOperationException {
		ItemStack backpack = new ItemStack(testBackpack);
		BackpackItem.setColors(backpack, 0xFF112233, 0xFF445566);
		backpack.getOrCreateTag().putUUID(BackpackWrapper.CONTENTS_UUID_TAG, UUID.randomUUID());
		CompoundTag loadedTag = new CompoundTag();
		loadedTag.put(BackpackBlockEntity.BACKPACK_DATA_TAG, backpack.save(new CompoundTag()));
		BackpackBlockEntity blockEntity = uninitializedBackpackBlockEntity();

		blockEntity.load(loadedTag);
		assertEquals(0xFF112233, blockEntity.getMainColor());
		assertEquals(0xFF445566, blockEntity.getAccentColor());
		CompoundTag savedTag = blockEntity.saveWithoutMetadata();
		ItemStack savedBackpack = ItemStack.of(savedTag.getCompound(BackpackBlockEntity.BACKPACK_DATA_TAG));

		assertTrue(ItemStack.matches(backpack, savedBackpack));
	}

	private static BackpackBlockEntity uninitializedBackpackBlockEntity() throws ReflectiveOperationException {
		Field unsafeField = Unsafe.class.getDeclaredField("theUnsafe");
		unsafeField.setAccessible(true);
		return (BackpackBlockEntity) ((Unsafe) unsafeField.get(null)).allocateInstance(BackpackBlockEntity.class);
	}

	private static BackpackItem registerBackpack(String name, int inventorySlots, int upgradeSlots) {
		setItemRegistryFrozen(false);
		BackpackItem backpack = new BackpackItem(() -> inventorySlots, () -> upgradeSlots, () -> null);
		Registry.register(BuiltInRegistries.ITEM, "sophisticatedbackpacks:" + name, backpack);
		setItemRegistryFrozen(true);
		return backpack;
	}

	private static void setItemRegistryFrozen(boolean frozen) {
		boolean foundFrozen = false;
		boolean foundLocked = false;
		for (Class<?> type = BuiltInRegistries.ITEM.getClass(); type != null; type = type.getSuperclass()) {
			for (String name : List.of("frozen", "locked")) {
				try {
					Field field = type.getDeclaredField(name);
					field.setAccessible(true);
					field.setBoolean(BuiltInRegistries.ITEM, frozen);
					foundFrozen |= name.equals("frozen");
					foundLocked |= name.equals("locked");
				} catch (NoSuchFieldException ignored) {
					// The Forge registry wrapper owns these guards on 1.20.1.
				} catch (IllegalAccessException e) {
					throw new IllegalStateException("Unable to register serialized backpack test fixture", e);
				}
			}
		}
		if (!foundFrozen || !foundLocked) {
			throw new IllegalStateException("Unable to locate the item registry write guards");
		}
	}
}
