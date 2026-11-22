package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.LivingEntity;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutPart;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class MobCatcherStorageTest {
	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
	}

	@Test
	void villagerSizedMobKeepsFootprintCloseToAabbRatioWithoutExcessiveOverfill() {
		assertEquals(new CapturedMobFootprint(3, 7), MobCatcherStorage.getFootprint(entityWithSize(0.6F, 1.95F), 20));
	}

	@Test
	void cowSizedMobUsesModeratelyWideFootprint() {
		assertEquals(new CapturedMobFootprint(4, 5), MobCatcherStorage.getFootprint(entityWithSize(0.9F, 1.4F), 20));
	}

	@Test
	void chickenSizedMobStaysCompact() {
		assertEquals(new CapturedMobFootprint(2, 2), MobCatcherStorage.getFootprint(entityWithSize(0.4F, 0.7F), 4));
	}

	@Test
	void tallHostileSizedMobCanStillUseTallFootprint() {
		assertEquals(new CapturedMobFootprint(4, 10), MobCatcherStorage.getFootprint(entityWithSize(0.6F, 1.95F), 40));
	}

	@Test
	void targetSlotKeepsCapturedMobWithinTargetBoundsWhenColumnsShrink() {
		assertEquals(5, MobCatcherStorage.getTargetSlot(capturedMob(7, 2, 2), 9, 7, 18));
	}

	@Test
	void getTargetSlotTranslatesCapturedMobCoordinatesFromNineToTwelveColumns() {
		assertEquals(20, MobCatcherStorage.getTargetSlot(capturedMob(17, 2, 2), 9, 12, 81));
	}

	@Test
	void getColumnsReconstructsPartialRowBaseSlotsAfterColumnsAreTaken() {
		IBackpackWrapper backpackWrapper = mock(IBackpackWrapper.class);
		InventoryHandler inventoryHandler = mock(InventoryHandler.class);
		when(backpackWrapper.getInventoryHandler()).thenReturn(inventoryHandler);
		when(backpackWrapper.getColumnsTaken()).thenReturn(2);
		when(backpackWrapper.getNumberOfSlotRows()).thenReturn(7);
		when(inventoryHandler.size()).thenReturn(68);

		int columns = MobCatcherStorage.getColumns(backpackWrapper);

		assertEquals(10, columns);
	}

	@Test
	void fitKeepsDifferentlySizedCapturedFootprintsSeparateAfterColumnChange() {
		List<InventoryLayoutPart> parts = List.of(new InventoryLayoutPart("mob:small", 7, 2, 2, Set.of(7, 8, 16, 17)),
				new InventoryLayoutPart("mob:large", 8, 3, 2, Set.of(18, 19, 20, 27, 28, 29)), new InventoryLayoutPart("stack:30", 24, 1, 1, Set.of(30)));

		var result = InventoryLayoutFitter.fit(parts, 108, 12);

		assertTrue(result.fits());
		assertTrue(parts.stream().anyMatch(part -> result.fittedSlots().get(part.id()) != part.firstSlot()));
		Set<Integer> occupiedSlots = new HashSet<>();
		for (InventoryLayoutPart part : parts) {
			int fittedSlot = result.fittedSlots().get(part.id());
			for (int y = 0; y < part.height(); y++) {
				for (int x = 0; x < part.width(); x++) {
					int slot = fittedSlot + y * 12 + x;
					assertTrue(slot >= 0 && slot < 108);
					assertFalse(occupiedSlots.contains(slot));
					occupiedSlots.add(slot);
				}
			}
		}
	}

	private CapturedMob capturedMob(int slot, int width, int height) {
		return new CapturedMob(new UUID(0, 1), Identifier.parse("minecraft:pig"), new CompoundTag(), slot, width, height, width * height, false,
				"minecraft:pig", 10, 10);
	}

	private LivingEntity entityWithSize(float width, float height) {
		LivingEntity entity = mock(LivingEntity.class);
		when(entity.getBbWidth()).thenReturn(width);
		when(entity.getBbHeight()).thenReturn(height);
		return entity;
	}
}
