package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.SharedConstants;
import net.minecraft.core.BlockPos;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageHostWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageBlockEndpoint;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageEndpointAdapter;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageHostFactories;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class BackpackBlockEntityTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
	}

	@Test
	void getLinkedStorageBlockEndpointAdapterAcceptsStorageHostsLikeTheBackpackItemAdapter() {
		BackpackBlockEntity blockEntity = new BackpackBlockEntity(BlockPos.ZERO, ModBlocks.BACKPACK.get().defaultBlockState());
		LinkedStorageHostFactories.registerCompatibility(BackpackLinkedStorageHostWrapper.FACTORY_ID, ResourceLocation.parse("sophisticatedstorage:storage"));

		ILinkedStorageEndpointAdapter<ILinkedStorageBlockEndpoint> adapter = blockEntity.getLinkedStorageBlockEndpointAdapter();

		assertTrue(adapter.acceptsHostFactory(ResourceLocation.parse("sophisticatedstorage:storage")));
		assertTrue(adapter.acceptsHostFactory(ResourceLocation.parse("sophisticatedbackpacks:backpack")));
		assertFalse(adapter.acceptsHostFactory(ResourceLocation.parse("other:host")));
	}

	@Test
	void loadAdditionalPreservesPendingBackpackAndTints() {
		ItemStack backpack = new ItemStack(ModItems.NETHERITE_BACKPACK.get());
		backpack.set(ModCoreDataComponents.MAIN_COLOR, 0xFF112233);
		backpack.set(ModCoreDataComponents.ACCENT_COLOR, 0xFF445566);
		backpack.set(ModCoreDataComponents.STORAGE_UUID, UUID.randomUUID());
		CompoundTag loadedTag = new CompoundTag();
		loadedTag.put(BackpackBlockEntity.BACKPACK_DATA_TAG, backpack.save(REGISTRY_ACCESS));
		BackpackBlockEntity blockEntity = new BackpackBlockEntity(BlockPos.ZERO, ModBlocks.BACKPACK.get().defaultBlockState());

		blockEntity.loadAdditional(loadedTag, REGISTRY_ACCESS);
		assertEquals(0xFF112233, blockEntity.getMainColor());
		assertEquals(0xFF445566, blockEntity.getAccentColor());
		CompoundTag savedTag = blockEntity.saveCustomOnly(REGISTRY_ACCESS);
		ItemStack savedBackpack = ItemStack.parseOptional(REGISTRY_ACCESS, savedTag.getCompound(BackpackBlockEntity.BACKPACK_DATA_TAG));

		assertTrue(ItemStack.isSameItemSameComponents(backpack, savedBackpack));
	}
}
