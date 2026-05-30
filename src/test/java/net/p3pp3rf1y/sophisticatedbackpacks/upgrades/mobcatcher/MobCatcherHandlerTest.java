package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class MobCatcherHandlerTest {
	@Test
	void effectiveMaxHealthUsesCurrentHealthWhenAttributeValueDoesNotIncludeModifier() {
		LivingEntity entity = mock(LivingEntity.class);
		when(entity.getAttributeValue(Attributes.MAX_HEALTH)).thenReturn(20D);
		when(entity.getMaxHealth()).thenReturn(20F);
		when(entity.getHealth()).thenReturn(40F);

		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(entity));
	}

	@Test
	void effectiveMaxHealthUsesGetMaxHealthWhenItIncludesModifier() {
		LivingEntity entity = mock(LivingEntity.class);
		when(entity.getAttributeValue(Attributes.MAX_HEALTH)).thenReturn(20D);
		when(entity.getMaxHealth()).thenReturn(40F);
		when(entity.getHealth()).thenReturn(10F);

		assertEquals(40D, MobCatcherHandler.getEffectiveMaxHealth(entity));
	}
}
