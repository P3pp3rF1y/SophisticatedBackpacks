package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.NoopStorageWrapper;

public interface IBackpackWrapper extends IStorageWrapper {
	@Override
	BackpackSettingsHandler getSettingsHandler();

	ItemStack getBackpack();

	ItemStack cloneBackpack();

	class Noop extends NoopStorageWrapper implements IBackpackWrapper {
		public static final Noop INSTANCE = new Noop();

		private final ItemStack backpack = new ItemStack(ModItems.BACKPACK.get());
		private final BackpackSettingsHandler settingsHandler = new BackpackSettingsHandler(this, new CompoundTag(), () -> {});

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
	}
}
