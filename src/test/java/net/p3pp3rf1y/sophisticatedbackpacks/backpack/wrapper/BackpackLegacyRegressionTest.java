package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.electronwill.nightconfig.core.CommentedConfig;
import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.server.Bootstrap;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.entity.EntityEquipment;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.fml.config.IConfigSpec;
import net.neoforged.fml.config.ModConfig;
import net.neoforged.neoforge.common.ModConfigSpec;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitResult;
import net.p3pp3rf1y.sophisticatedcore.api.InventoryLayoutFitter;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.lang.reflect.Constructor;
import java.nio.file.Path;
import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertFalse;

class BackpackLegacyRegressionTest {
	@BeforeAll
	static void setup() throws ReflectiveOperationException {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		DataComponentMap components = DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build();
		for (Item item : List.of(ModItems.GOLD_BACKPACK.get(), ModItems.DIAMOND_BACKPACK.get(), ModItems.NETHERITE_BACKPACK.get())) {
			item.builtInRegistryHolder().bindComponents(components);
		}
		loadDefaultConfig(Config.SERVER_SPEC);
	}

	private static void loadDefaultConfig(ModConfigSpec configSpec) throws ReflectiveOperationException {
		if (configSpec.isLoaded()) {
			return;
		}
		CommentedConfig config = CommentedConfig.inMemory();
		configSpec.correct(config);
		Class<?> loadedConfigClass = Class.forName("net.neoforged.fml.config.LoadedConfig");
		Constructor<?> constructor = loadedConfigClass.getDeclaredConstructor(CommentedConfig.class, Path.class, ModConfig.class);
		constructor.setAccessible(true);
		configSpec.acceptConfig((IConfigSpec.ILoadedConfig) constructor.newInstance(config, null, null));
	}

	@Test
	void inventoryTickCompletesDeliveredTierUpgradeOnceAndRemovesMarkers() {
		UUID operationId = UUID.randomUUID();
		ItemStack delivered = new ItemStack(ModItems.DIAMOND_BACKPACK.get());
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get(), 9);
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get(), 81);
		delivered.set(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get(), 4);
		delivered.set(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get(), operationId);
		Player player = Mockito.mock(Player.class);
		Inventory inventory = new Inventory(player, Mockito.mock(EntityEquipment.class));
		inventory.getNonEquipmentItems().set(0, delivered);
		AbstractContainerMenu menu = Mockito.mock(AbstractContainerMenu.class);
		player.containerMenu = menu;
		Mockito.when(menu.getCarried()).thenReturn(ItemStack.EMPTY);
		Mockito.when(player.getInventory()).thenReturn(inventory);
		Mockito.when(player.isSpectator()).thenReturn(false);
		Mockito.when(player.isDeadOrDying()).thenReturn(false);
		ServerLevel level = Mockito.mock(ServerLevel.class);
		IBackpackWrapper wrapper = Mockito.mock(IBackpackWrapper.class);
		BackpackWrapper source = Mockito.mock(BackpackWrapper.class);
		InventoryHandler inventoryHandler = Mockito.mock(InventoryHandler.class);
		InventoryLayoutFitResult fitResult = Mockito.mock(InventoryLayoutFitResult.class);
		Mockito.when(wrapper.getInventoryHandler()).thenReturn(inventoryHandler);
		Mockito.when(inventoryHandler.size()).thenReturn(108);
		Mockito.when(wrapper.getColumnsTaken()).thenReturn(0);
		Mockito.when(wrapper.getNumberOfSlotRows()).thenReturn(9);
		Mockito.when(source.getInventoryLayoutParts(9, 12)).thenReturn(List.of());
		Mockito.when(fitResult.fits()).thenReturn(true);
		boolean onlyWornBefore = Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.get();

		try (MockedStatic<BackpackWrapper> wrappers = Mockito.mockStatic(BackpackWrapper.class);
				MockedStatic<InventoryLayoutFitter> fitter = Mockito.mockStatic(InventoryLayoutFitter.class)) {
			wrappers.when(() -> BackpackWrapper.fromStack(delivered)).thenReturn(wrapper);
			wrappers.when(() -> BackpackWrapper.createDetachedInventoryLayoutView(delivered, 81, 4)).thenReturn(source);
			fitter.when(() -> InventoryLayoutFitter.fit(Mockito.anyList(), Mockito.anyInt(), Mockito.anyInt())).thenReturn(fitResult);
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(true);

			ModItems.DIAMOND_BACKPACK.get().inventoryTick(delivered, level, player, null);
			ModItems.DIAMOND_BACKPACK.get().inventoryTick(delivered, level, player, null);
		} finally {
			Config.SERVER.nerfsConfig.onlyWornBackpackTriggersUpgrades.set(onlyWornBefore);
		}

		Mockito.verify(wrapper).applyInventoryLayout(fitResult, 12);
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_COLUMNS.get()));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_INVENTORY_SLOTS.get()));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_SOURCE_UPGRADE_SLOTS.get()));
		assertFalse(delivered.has(ModDataComponents.TIER_UPGRADE_OPERATION_ID.get()));
	}
}
