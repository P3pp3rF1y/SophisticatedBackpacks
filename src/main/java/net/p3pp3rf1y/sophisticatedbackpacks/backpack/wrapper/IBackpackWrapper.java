package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.NoopStorageWrapper;

import java.util.UUID;
import java.util.function.IntConsumer;

public interface IBackpackWrapper extends IStorageWrapper {

	@Override
	BackpackSettingsHandler getSettingsHandler();

	ItemStack getBackpack();

	ItemStack cloneBackpack();

	void copyDataTo(IStorageWrapper otherStorageWrapper);

	void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots);

	void setLoot(ResourceLocation lootTableName, float lootPercentage);

	void setContentsUuid(UUID storageUuid);

	default void removeContentsUuid() {
		//noop by default
	}

	default void removeContentsUUIDTag() {
		//noop
	}

	default void registerOnSlotsChangeListener(IntConsumer onSlotsChange) {
		//noop
	}

	default void unregisterOnSlotsChangeListener() {
		//noop
	}

	default void registerOnInventoryHandlerRefreshListener(Runnable onInventoryHandlerRefresh) {
		//noop
	}

	default void unregisterOnInventoryHandlerRefreshListener() {
		//noop
	}

	class Noop extends NoopStorageWrapper implements IBackpackWrapper {
		public static final Noop INSTANCE = new Noop();

		private final ItemStack backpack = new ItemStack(ModItems.BACKPACK.get());
		private final BackpackSettingsHandler settingsHandler = new BackpackSettingsHandler(this, new CompoundTag(), () -> {
		});

		@Override
		public BackpackSettingsHandler getSettingsHandler() {
			return settingsHandler;
		}

		@Override
		public ItemStack getBackpack() {
			return backpack;
		}

		@Override
		public ItemStack cloneBackpack() {
			return backpack;
		}

		@Override
		public void copyDataTo(IStorageWrapper otherStorageWrapper) {
			//noop
		}

		@Override
		public void setSlotNumbers(int numberOfInventorySlots, int numberOfUpgradeSlots) {
			//noop
		}

		@Override
		public void setLoot(ResourceLocation lootTableName, float lootPercentage) {
			//noop
		}

		@Override
		public void setContentsUuid(UUID storageUuid) {
			//noop
		}
	}
}
