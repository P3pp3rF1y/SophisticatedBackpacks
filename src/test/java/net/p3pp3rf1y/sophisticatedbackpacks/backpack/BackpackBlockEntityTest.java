package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.Identifier;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.util.ValueIOHelper;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackBlockEntityTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
	private static BackpackItem testBackpack;

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		testBackpack = (BackpackItem) BuiltInRegistries.ITEM.getValue(Identifier.fromNamespaceAndPath("sophisticatedbackpacks", "backpack"));
		testBackpack.builtInRegistryHolder().bindComponents(DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 1).build());
		Items.AIR.builtInRegistryHolder().bindComponents(DataComponentMap.EMPTY);
	}

	@Test
	void saveAdditionalPreservesPendingLoadedBackpack() {
		ItemStack backpack = new ItemStack(testBackpack);
		backpack.set(ModCoreDataComponents.MAIN_COLOR, 0xFF112233);
		backpack.set(ModCoreDataComponents.ACCENT_COLOR, 0xFF445566);
		backpack.set(ModCoreDataComponents.STORAGE_UUID, UUID.randomUUID());
		CompoundTag loadedTag = ValueIOHelper.collectOutputToTag(REGISTRY_ACCESS,
				out -> out.store(BackpackBlockEntity.BACKPACK_DATA, ItemStack.CODEC, backpack));
		BackpackBlockEntity blockEntity = new BackpackBlockEntity(BlockPos.ZERO, ModBlocks.BACKPACK.get().defaultBlockState());

		blockEntity.loadAdditional(ValueIOHelper.inputFromCompoundTag(REGISTRY_ACCESS, loadedTag));
		CompoundTag savedTag = blockEntity.saveCustomOnly(REGISTRY_ACCESS);
		ItemStack savedBackpack = ValueIOHelper.inputFromCompoundTag(REGISTRY_ACCESS, savedTag).read(BackpackBlockEntity.BACKPACK_DATA, ItemStack.CODEC)
				.orElse(ItemStack.EMPTY);

		assertTrue(ItemStack.isSameItemSameComponents(backpack, savedBackpack));
	}
}
