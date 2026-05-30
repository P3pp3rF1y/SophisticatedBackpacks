package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class MobCatcherHandlerTest {
	@Test
	void effectiveMaxHealthUsesCurrentHealthWhenAttributeValueDoesNotIncludeModifier() {
		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(20D, 20F, 40F));
	}

	@Test
	void effectiveMaxHealthUsesGetMaxHealthWhenItIncludesModifier() {
		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(20D, 40F, 10F));
	}
}
