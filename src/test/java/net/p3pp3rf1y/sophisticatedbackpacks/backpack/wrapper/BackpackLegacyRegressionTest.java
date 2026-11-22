package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.Registry;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Field;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNull;

class BackpackLegacyRegressionTest {
	private static BackpackItem DIAMOND;

	@BeforeAll
	static void bootstrapRegistries() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		DIAMOND = registerBackpack("tier_regression_diamond", 108, 8);
		if (!Config.SERVER_SPEC.isLoaded()) {
			CommentedConfig config = CommentedConfig.inMemory();
			Config.SERVER_SPEC.correct(config);
			Config.SERVER_SPEC.acceptConfig(config);
		}
	}

	@Test
	void inventoryTickCompletesDeliveredTierUpgradeOnceAndRemovesMarkers() {
		ItemStack delivered = markedBackpack();
		Player player = playerWith(delivered);
		ServerLevel level = Mockito.mock(ServerLevel.class);
		InventoryLayoutFitResult fitResult = Mockito.mock(InventoryLayoutFitResult.class);
		BackpackWrapper source = Mockito.mock(BackpackWrapper.class);
		BackpackWrapper target = Mockito.mock(BackpackWrapper.class);
		InventoryHandler inventoryHandler = Mockito.mock(InventoryHandler.class);
		Mockito.when(source.getInventoryLayoutParts(9, 12)).thenReturn(List.of());
		Mockito.when(target.getInventoryHandler()).thenReturn(inventoryHandler);
		Mockito.when(inventoryHandler.getSlots()).thenReturn(108);
		boolean onlyWornBefore = Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get();

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class);
				MockedStatic<BackpackLinkedStorageResolver> resolver = Mockito.mockStatic(BackpackLinkedStorageResolver.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			wrappers.when(() -> BackpackWrapper.createDetachedInventoryLayoutView(delivered, 81, 4)).thenReturn(source);
			resolver.when(() -> BackpackLinkedStorageResolver.resolveOrCreate(level, delivered)).thenReturn(target);
			Mockito.when(fitResult.fits()).thenReturn(true);
			fitter.when(() -> InventoryLayoutFitter.fit(Mockito.anyList(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(fitResult);
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(true);

			DIAMOND.inventoryTick(delivered, level, player, 0, false);
			DIAMOND.inventoryTick(delivered, level, player, 0, false);

			Mockito.verify(target).applyInventoryLayout(fitResult, 12);
			wrappers.verify(() -> BackpackWrapper.createDetachedInventoryLayoutView(delivered, 81, 4));
		} finally {
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(onlyWornBefore);
		}

		assertNull(ModDataComponents.getSourceColumns(delivered));
		assertNull(ModDataComponents.getSourceInventorySlots(delivered));
		assertNull(ModDataComponents.getSourceUpgradeSlots(delivered));
		assertNull(ModDataComponents.getOperationId(delivered));
	}

	private static ItemStack markedBackpack() {
		ItemStack stack = new ItemStack(DIAMOND);
		stack.getOrCreateTag().putInt("inventorySlots", 108);
		ModDataComponents.setSourceColumns(stack, 9);
		ModDataComponents.setSourceInventorySlots(stack, 81);
		ModDataComponents.setSourceUpgradeSlots(stack, 4);
		ModDataComponents.setOperationId(stack, UUID.randomUUID());
		return stack;
	}

	private static Player playerWith(ItemStack delivered) {
		Player player = Mockito.mock(Player.class);
		Inventory inventory = new Inventory(player);
		inventory.items.set(0, delivered);
		AbstractContainerMenu menu = Mockito.mock(AbstractContainerMenu.class);
		player.containerMenu = menu;
		Mockito.when(menu.getCarried()).thenReturn(ItemStack.EMPTY);
		Mockito.when(player.getInventory()).thenReturn(inventory);
		return player;
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
					throw new IllegalStateException("Unable to register backpack test fixture", e);
				}
			}
		}
		if (!foundFrozen || !foundLocked) {
			throw new IllegalStateException("Unable to locate the item registry write guards");
		}
	}
}
