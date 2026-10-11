package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class MobCatcherHandlerTest {
	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		ModItems.BACKPACK.get().builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
	}

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

	@Test
	void tryCaptureConsumesProjectedPrimaryClientInteractionWithoutContentsSnapshot() {
		ItemStack backpack = linkedEndpoint();
		backpack.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, true);
		backpack.set(ModDataComponents.LINKED_MOB_CATCHER, true);
		Player player = mock(Player.class);
		Level level = mock(Level.class);
		when(level.isClientSide()).thenReturn(true);
		when(player.level()).thenReturn(level);
		when(player.getItemInHand(InteractionHand.MAIN_HAND)).thenReturn(backpack);
		try (MockedStatic<BackpackLinkedStorageResolver> resolver = mockStatic(BackpackLinkedStorageResolver.class)) {
			assertEquals(InteractionResult.SUCCESS, MobCatcherHandler.tryCapture(player, InteractionHand.MAIN_HAND, mock(LivingEntity.class)));
			resolver.verifyNoInteractions();
		}
	}

	@Test
	void tryCaptureDoesNotPredictCaptureForSecondaryEvenWithStaleProjection() {
		ItemStack backpack = linkedEndpoint();
		backpack.set(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT, false);
		backpack.set(ModDataComponents.LINKED_MOB_CATCHER, true);
		Player player = mock(Player.class);
		Level level = mock(Level.class);
		when(level.isClientSide()).thenReturn(true);
		when(player.level()).thenReturn(level);
		when(player.getItemInHand(InteractionHand.MAIN_HAND)).thenReturn(backpack);
		try (MockedStatic<BackpackLinkedStorageResolver> resolver = mockStatic(BackpackLinkedStorageResolver.class)) {
			resolver.when(() -> BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, backpack)).thenReturn(IBackpackWrapper.Noop.INSTANCE);
			assertEquals(InteractionResult.PASS, MobCatcherHandler.tryCapture(player, InteractionHand.MAIN_HAND, mock(LivingEntity.class)));
		}
	}

	private static ItemStack linkedEndpoint() {
		ItemStack backpack = new ItemStack(ModItems.BACKPACK.get());
		backpack.set(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT, new LinkedStorageEndpointData(UUID.randomUUID(), UUID.randomUUID()));
		return backpack;
	}
}
