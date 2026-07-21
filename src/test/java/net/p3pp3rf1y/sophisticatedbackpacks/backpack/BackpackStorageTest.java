package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.mojang.serialization.Lifecycle;
import net.minecraft.SharedConstants;
import net.minecraft.core.Holder;
import net.minecraft.core.HolderLookup;
import net.minecraft.core.HolderSet;
import net.minecraft.core.Registry;
import net.minecraft.core.RegistryAccess;
import net.minecraft.core.UUIDUtil;
import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponents;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.Tag;
import net.minecraft.resources.RegistryOps;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.Bootstrap;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.enchantment.Enchantment;
import net.minecraft.world.item.enchantment.Enchantments;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.util.Optional;
import java.util.UUID;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BackpackStorageTest {
	private static final RegistryAccess REGISTRY_ACCESS = RegistryAccess.fromRegistryOfRegistries(BuiltInRegistries.REGISTRY);
	private static final HolderLookup.RegistryLookup<Enchantment> ENCHANTMENT_LOOKUP = new HolderLookup.RegistryLookup<>() {
		private final Holder.Reference<Enchantment> sharpness = Holder.Reference.createStandAlone(this, Enchantments.SHARPNESS);

		@Override
		public ResourceKey<? extends Registry<? extends Enchantment>> key() {
			return Registries.ENCHANTMENT;
		}

		@Override
		public Lifecycle registryLifecycle() {
			return Lifecycle.stable();
		}

		@Override
		public Optional<Holder.Reference<Enchantment>> get(ResourceKey<Enchantment> key) {
			return key.equals(Enchantments.SHARPNESS) ? Optional.of(sharpness) : Optional.empty();
		}

		@Override
		public Stream<Holder.Reference<Enchantment>> listElements() {
			return Stream.of(sharpness);
		}

		@Override
		public Optional<HolderSet.Named<Enchantment>> get(TagKey<Enchantment> tag) {
			return Optional.empty();
		}

		@Override
		public Stream<HolderSet.Named<Enchantment>> listTags() {
			return Stream.empty();
		}
	};

	@BeforeAll
	static void setup() {
		SharedConstants.tryDetectVersion();
		Bootstrap.bootStrap();
		Bootstrap.validate();
		bindTestComponents(Items.DIAMOND_SWORD);
	}

	private static void bindTestComponents(Item... items) {
		DataComponentMap components = DataComponentMap.builder().set(DataComponents.MAX_STACK_SIZE, 64).build();
		for (Item item : items) {
			item.builtInRegistryHolder().bindComponents(components);
		}
	}

	@Test
	void legacyBackpackStorageListsDeserializeToCurrentStorage() {
		UUID backpackUuid = new UUID(1, 2);
		CompoundTag legacyStorage = new CompoundTag();
		legacyStorage.put("accessLogRecords", legacyAccessLogs(backpackUuid));
		legacyStorage.put("backpackContents", legacyBackpackContents(backpackUuid));

		BackpackStorage storage = BackpackStorage.legacyDeserialize(legacyStorage);

		assertEquals(1, storage.getAccessLogs().size());
		assertEquals("Player", storage.getAccessLogs().get(backpackUuid).playerName());
		assertEquals(2, storage.getOrCreateBackpackContents(backpackUuid).inventory().stacks().size());
	}

	@Test
	void legacyBackpackStorageKeepsEnchantedItems() {
		UUID backpackUuid = new UUID(1, 2);
		CompoundTag legacyStorage = new CompoundTag();
		CompoundTag backpackContents = new CompoundTag();
		backpackContents.put(backpackUuid.toString(), enchantedContents());
		legacyStorage.put("backpackContents", backpackContents);
		legacyStorage.put("accessLogRecords", new CompoundTag());

		BackpackStorage storage = BackpackStorage.deserialize(legacyStorage, registryOps()).orElseThrow();

		assertEquals(Items.DIAMOND_SWORD, storage.getOrCreateBackpackContents(backpackUuid).inventory().stacks().get(0).getItem());
	}

	private static CompoundTag enchantedContents() {
		CompoundTag contents = (CompoundTag) ContainerContents.CODEC.encodeStart(registryOps(), new ContainerContents()).getOrThrow();
		CompoundTag stack = new CompoundTag();
		stack.putString("id", "minecraft:diamond_sword");
		stack.putInt("count", 1);
		CompoundTag enchantments = new CompoundTag();
		enchantments.putInt("minecraft:sharpness", 5);
		CompoundTag components = new CompoundTag();
		components.put("minecraft:enchantments", enchantments);
		stack.put("components", components);
		ListTag stacks = new ListTag();
		stacks.add(stack);
		contents.getCompound("inventory").orElseThrow().put("stacks", stacks);
		return contents;
	}

	private static RegistryOps<Tag> registryOps() {
		return RegistryOps.create(NbtOps.INSTANCE, BackpackStorageTest::lookupRegistry);
	}

	@SuppressWarnings("unchecked")
	private static <T> Optional<RegistryOps.RegistryInfo<T>> lookupRegistry(ResourceKey<? extends Registry<? extends T>> registryKey) {
		if (registryKey.equals(Registries.ENCHANTMENT)) {
			return Optional.of((RegistryOps.RegistryInfo<T>) RegistryOps.RegistryInfo.fromRegistryLookup(ENCHANTMENT_LOOKUP));
		}
		return REGISTRY_ACCESS.lookup(registryKey).map(RegistryOps.RegistryInfo::fromRegistryLookup);
	}

	private static ListTag legacyAccessLogs(UUID backpackUuid) {
		ListTag accessLogs = new ListTag();
		CompoundTag accessLog = new CompoundTag();
		accessLog.putString("backpackItemRegistryName", "sophisticatedbackpacks:backpack");
		accessLog.put("backpackUuid", UUIDUtil.CODEC.encodeStart(NbtOps.INSTANCE, backpackUuid).getOrThrow());
		accessLog.putString("playerName", "Player");
		accessLog.putString("backpackName", "Backpack");
		accessLog.putInt("clothColor", -1);
		accessLog.putInt("trimColor", -1);
		accessLog.putLong("accessTime", 123L);
		accessLog.putInt("columnsTaken", 9);
		accessLogs.add(accessLog);
		return accessLogs;
	}

	private static ListTag legacyBackpackContents(UUID backpackUuid) {
		ListTag backpackContents = new ListTag();
		CompoundTag uuidContentsPair = new CompoundTag();
		uuidContentsPair.put("uuid", UUIDUtil.CODEC.encodeStart(NbtOps.INSTANCE, backpackUuid).getOrThrow());
		CompoundTag contents = new CompoundTag();
		CompoundTag inventory = new CompoundTag();
		inventory.putInt("Size", 2);
		inventory.put("Items", new ListTag());
		contents.put("inventory", inventory);
		uuidContentsPair.put("contents", contents);
		backpackContents.add(uuidContentsPair);
		return backpackContents;
	}
}
