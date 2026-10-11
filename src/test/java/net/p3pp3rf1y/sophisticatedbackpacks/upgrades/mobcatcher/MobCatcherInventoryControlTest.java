package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.SharedConstants;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class MobCatcherInventoryControlTest {
	@BeforeAll
	static void bootstrap() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void createRendersCanonicalBackpackMobsThroughNonBackpackMenu() {
		StorageScreenBase<?> screen = mock(StorageScreenBase.class);
		StorageContainerMenuBase<?> menu = mock(StorageContainerMenuBase.class);
		IStorageWrapper endpoint = mock(IStorageWrapper.class);
		IBackpackWrapper host = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		MobCatcherUpgradeWrapper upgrade = mock(MobCatcherUpgradeWrapper.class);
		doReturn(menu).when(screen).getMenu();
		doReturn(endpoint).when(menu).getStorageWrapper();
		when(endpoint.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getTypeWrappers(MobCatcherUpgradeItem.TYPE)).thenReturn(List.of(upgrade));
		when(upgrade.getBackpackWrapper()).thenReturn(Optional.of(host));
		when(menu.getNumberOfStorageInventorySlots()).thenReturn(9);
		CapturedMob mob = new CapturedMob(UUID.randomUUID(), Identifier.withDefaultNamespace("pig"), new CompoundTag(), 0, 1, 1, 1, false, "Pig", 10, 10);
		try (MockedStatic<MobCatcherStorage> storage = mockStatic(MobCatcherStorage.class)) {
			storage.when(() -> MobCatcherStorage.getCapturedMobs(host)).thenReturn(List.of(mob));
			storage.when(() -> MobCatcherStorage.getColumns(host)).thenReturn(9);
			Optional<MobCatcherInventoryControl> control = MobCatcherInventoryControl.create(screen);
			assertTrue(control.isPresent());
			assertTrue(control.orElseThrow().replacesSlotRender(0));
			assertFalse(control.orElseThrow().replacesSlotRender(1));
		}
	}

	@Test
	void createLeavesOrdinaryStorageWithoutMobCatcherUnchanged() {
		StorageScreenBase<?> screen = mock(StorageScreenBase.class);
		StorageContainerMenuBase<?> menu = mock(StorageContainerMenuBase.class);
		IStorageWrapper endpoint = mock(IStorageWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		doReturn(menu).when(screen).getMenu();
		doReturn(endpoint).when(menu).getStorageWrapper();
		when(endpoint.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getTypeWrappers(MobCatcherUpgradeItem.TYPE)).thenReturn(List.of());
		assertTrue(MobCatcherInventoryControl.create(screen).isEmpty());
	}
}
