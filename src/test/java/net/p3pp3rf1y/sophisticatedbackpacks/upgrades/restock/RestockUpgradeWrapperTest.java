package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.SharedConstants;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.core.component.DataComponents;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.neoforged.neoforge.transfer.item.ItemStacksResourceHandler;
import net.neoforged.neoforge.transfer.transaction.TransactionContext;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.inventory.ITrackedContentsItemResourceHandler;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterAttributes;
import net.p3pp3rf1y.sophisticatedcore.upgrades.PrimaryMatch;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class RestockUpgradeWrapperTest {
	private static final FilterAttributes BLOCK_LIST_FILTER_ATTRIBUTES = new FilterAttributes(Collections.emptySet(), false, false, false, PrimaryMatch.ITEM,
			true, net.minecraft.world.item.component.ItemContainerContents.EMPTY, false, false);

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		bindTestComponents(Items.DIAMOND, Items.IRON_INGOT);
	}

	private static void bindTestComponents(Item... items) {
		DataComponentMap components = DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build();
		for (Item item : items) {
			item.builtInRegistryHolder().bindComponents(components);
		}
	}

	@Test
	void restocksOnlyTheAmountActuallyExtracted() {
		ItemResource diamonds = ItemResource.of(Items.DIAMOND);
		LimitedExtractionHandler source = new LimitedExtractionHandler(diamonds, 64, 2);
		ITrackedContentsItemResourceHandler target = acceptingTarget();
		RestockUpgradeWrapper wrapper = createWrapper(target);

		List<ItemStack> transferred = wrapper.restockFromHandler(source);

		assertEquals(1, transferred.size());
		assertEquals(2, transferred.getFirst().getCount());
		assertEquals(62, source.getAmountAsInt(0));
		verify(target).insert(eq(diamonds), eq(2), any(TransactionContext.class));
	}

	@Test
	void skipsSourceSlotsRejectedByTheRestockFilter() {
		ItemResource diamonds = ItemResource.of(Items.DIAMOND);
		ItemResource iron = ItemResource.of(Items.IRON_INGOT);
		LimitedExtractionHandler source = new LimitedExtractionHandler(iron, 64, 64);
		ITrackedContentsItemResourceHandler target = acceptingTarget();
		RestockUpgradeWrapper wrapper = createWrapper(target);
		wrapper.getFilterLogic().setAllowList(true);
		wrapper.getFilterLogic().getFilterHandler().setStackInSlot(0, diamonds.toStack());

		List<ItemStack> transferred = wrapper.restockFromHandler(source);

		assertTrue(transferred.isEmpty());
		assertEquals(64, source.getAmountAsInt(0));
		verify(target, never()).insert(any(ItemResource.class), anyInt(), any(TransactionContext.class));
	}

	private static ITrackedContentsItemResourceHandler acceptingTarget() {
		ITrackedContentsItemResourceHandler target = mock(ITrackedContentsItemResourceHandler.class);
		when(target.insert(any(ItemResource.class), anyInt(), any(TransactionContext.class))).thenAnswer(invocation -> invocation.getArgument(1));
		return target;
	}

	private static RestockUpgradeWrapper createWrapper(ITrackedContentsItemResourceHandler target) {
		IStorageWrapper storageWrapper = mock(IStorageWrapper.class);
		when(storageWrapper.getInventoryForUpgradeProcessing()).thenReturn(target);
		when(storageWrapper.getSettingsHandler()).thenReturn(mock(SettingsHandler.class));

		RestockUpgradeItem upgradeItem = mock(RestockUpgradeItem.class);
		when(upgradeItem.getFilterSlotCount()).thenReturn(1);
		ItemStack upgrade = mock(ItemStack.class);
		Map<Object, Object> components = new HashMap<>();
		when(upgrade.getItem()).thenReturn(upgradeItem);
		when(upgrade.getOrDefault(anyDataComponentSupplier(), any())).thenAnswer(invocation -> {
			Supplier<?> component = invocation.getArgument(0);
			return components.getOrDefault(component, invocation.getArgument(1));
		});
		doAnswer(invocation -> components.put(invocation.getArgument(0), invocation.getArgument(1))).when(upgrade).set(anySetDataComponentSupplier(), any());

		return new RestockUpgradeWrapper(storageWrapper, upgrade, stack -> {
		});
	}

	@SuppressWarnings("unchecked")
	private static <T> Supplier<? extends DataComponentType<? extends T>> anyDataComponentSupplier() {
		return (Supplier<? extends DataComponentType<? extends T>>) any(Supplier.class);
	}

	@SuppressWarnings("unchecked")
	private static Supplier<? extends DataComponentType<Object>> anySetDataComponentSupplier() {
		return (Supplier<? extends DataComponentType<Object>>) any(Supplier.class);
	}

	private static class LimitedExtractionHandler extends ItemStacksResourceHandler {
		private final int extractionLimit;

		private LimitedExtractionHandler(ItemResource resource, int amount, int extractionLimit) {
			super(1);
			this.extractionLimit = extractionLimit;
			set(0, resource, amount);
		}

		@Override
		public int extract(int index, ItemResource resource, int amount, TransactionContext transaction) {
			return super.extract(index, resource, Math.min(amount, extractionLimit), transaction);
		}
	}
}
