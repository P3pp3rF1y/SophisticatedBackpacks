package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MobCatcherStorageTest {
	@Test
	void villagerSizedMobKeepsFootprintCloseToAabbRatioWithoutExcessiveOverfill() {
		assertEquals(new CapturedMobFootprint(3, 7), MobCatcherStorage.getFootprint(0.6F, 1.95F, 20));
	}

	@Test
	void cowSizedMobUsesModeratelyWideFootprint() {
		assertEquals(new CapturedMobFootprint(4, 5), MobCatcherStorage.getFootprint(0.9F, 1.4F, 20));
	}

	@Test
	void chickenSizedMobStaysCompact() {
		assertEquals(new CapturedMobFootprint(2, 2), MobCatcherStorage.getFootprint(0.4F, 0.7F, 4));
	}

	@Test
	void tallHostileSizedMobCanStillUseTallFootprint() {
		assertEquals(new CapturedMobFootprint(4, 10), MobCatcherStorage.getFootprint(0.6F, 1.95F, 40));
	}

	@Test
	void targetSlotKeepsCapturedMobWithinTargetBoundsWhenColumnsShrink() {
		assertEquals(5, MobCatcherStorage.getTargetSlot(capturedMob(7, 2, 2), 9, 7, 18));
	}

	private CapturedMob capturedMob(int slot, int width, int height) {
		return new CapturedMob(new UUID(0, 1), new ResourceLocation("minecraft:pig"), new CompoundTag(), slot, width, height, width * height, false,
				"minecraft:pig", 10, 10);
	}
}
