package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class MobCatcherInventoryControlTest {
	@BeforeAll
	static void bootstrap() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void findsCanonicalBackpackMobsThroughForeignFamilyWrapper() {
		IStorageWrapper endpoint = mock(IStorageWrapper.class);
		IBackpackWrapper host = mock(IBackpackWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		MobCatcherUpgradeWrapper upgrade = mock(MobCatcherUpgradeWrapper.class);
		when(endpoint.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getTypeWrappers(MobCatcherUpgradeItem.TYPE)).thenReturn(List.of(upgrade));
		when(upgrade.getBackpackWrapper()).thenReturn(Optional.of(host));

		assertSame(host, MobCatcherInventoryControl.findCanonicalBackpack(endpoint).orElseThrow());
	}

	@Test
	void leavesOrdinaryStorageWithoutMobCatcherUnchanged() {
		IStorageWrapper endpoint = mock(IStorageWrapper.class);
		UpgradeHandler upgrades = mock(UpgradeHandler.class);
		when(endpoint.getUpgradeHandler()).thenReturn(upgrades);
		when(upgrades.getTypeWrappers(MobCatcherUpgradeItem.TYPE)).thenReturn(List.of());

		assertTrue(MobCatcherInventoryControl.findCanonicalBackpack(endpoint).isEmpty());
	}
}
