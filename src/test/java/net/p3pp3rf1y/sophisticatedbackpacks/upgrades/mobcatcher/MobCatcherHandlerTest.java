package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MobCatcherHandlerTest {
	@BeforeAll
	static void bootstrap() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void projectedPrimaryPredictsMobCatcherWithoutLoadingCanonicalContents() {
		ItemStack stack = new ItemStack(Items.CHEST);
		LinkedStorageStackData.setEndpoint(stack, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		LinkedStorageStackData.setPrimaryEndpoint(stack, true);
		stack.getOrCreateTag().putBoolean("linked_mob_catcher", true);
		assertTrue(MobCatcherHandler.isProjectedPrimaryMobCatcher(stack));
		LinkedStorageStackData.setPrimaryEndpoint(stack, false);
		assertFalse(MobCatcherHandler.isProjectedPrimaryMobCatcher(stack));
		LinkedStorageStackData.setPrimaryEndpoint(stack, true);
		stack.getOrCreateTag().putBoolean("linked_mob_catcher", false);
		assertFalse(MobCatcherHandler.isProjectedPrimaryMobCatcher(stack));
	}

	@Test
	void effectiveMaxHealthUsesCurrentHealthWhenAttributeValueDoesNotIncludeModifier() {
		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(20D, 20F, 40F));
	}

	@Test
	void effectiveMaxHealthUsesGetMaxHealthWhenItIncludesModifier() {
		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(20D, 40F, 10F));
	}
}
