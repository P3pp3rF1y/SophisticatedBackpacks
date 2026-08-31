package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.RegistryAccess;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.IJukeboxPlaybackLocationProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;

import java.util.Optional;
import java.util.UUID;

public class BackpackLinkedStorageHostWrapper extends BackpackWrapper implements ILinkedStorageVirtualHost, IJukeboxPlaybackLocationProvider {
	public static final ResourceLocation FACTORY_ID = SophisticatedBackpacks.getRL("backpack");
	private final ILinkedStorageContentsBinding contents;

	public static BackpackLinkedStorageHostWrapper create(ILinkedStorageContentsBinding contents, CompoundTag virtualCarrier) {
		RegistryAccess registryAccess = RegistryHelper.getRegistryAccess()
				.orElseThrow(() -> new IllegalStateException("Registry access is required to create a linked storage backpack host"));
		return new BackpackLinkedStorageHostWrapper(contents, ItemStack.parse(registryAccess, virtualCarrier).orElseThrow());
	}

	public BackpackLinkedStorageHostWrapper(ILinkedStorageContentsBinding contents, ItemStack virtualCarrier) {
		super(requireBackpackVirtualCarrier(virtualCarrier), new LinkedStorageBackpackContentsSource(contents));
		this.contents = contents;
		BackpackItem backpackItem = (BackpackItem) getBackpack().getItem();
		getBackpack().set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS,
				getBackpack().getOrDefault(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots()));
		getBackpack().set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS,
				getBackpack().getOrDefault(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots()));
		configureRenderInfo();
	}

	private static ItemStack requireBackpackVirtualCarrier(ItemStack virtualCarrier) {
		if (!(virtualCarrier.getItem() instanceof BackpackItem)) {
			throw new IllegalArgumentException("Linked storage virtual carrier must be a Backpack");
		}
		return virtualCarrier;
	}

	private void configureRenderInfo() {
		getRenderInfo().setRenderDataChangeListener(contents::markRenderDirty);
	}

	@Override
	public int getMainColor() {
		return getBackpack().getOrDefault(ModCoreDataComponents.MAIN_COLOR, DEFAULT_MAIN_COLOR);
	}

	@Override
	public int getAccentColor() {
		return getBackpack().getOrDefault(ModCoreDataComponents.ACCENT_COLOR, DEFAULT_ACCENT_COLOR);
	}

	@Override
	public ItemStack cloneBackpack() {
		return getBackpack().copy();
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		// A linked-storage host identity belongs to its binding, never its carrier.
	}

	@Override
	public Optional<UUID> getContentsUuid() {
		return Optional.of(contents.groupId());
	}

	@Override
	public void setColumnsTaken(int columnsTaken, boolean hasChanged) {
		contents.setColumnsTaken(columnsTaken);
	}

	@Override
	public int getColumnsTaken() {
		return contents.getColumnsTaken();
	}

	@Override
	public void removeContentsUuid() {
		// A linked-storage host owns no BackpackStorage entry.
	}

	@Override
	public void removeContentsUUIDTag() {
		// The virtual carrier remains unlinked.
	}

	public void onLinkedStorageContentsChanged() {
		onContentsNbtUpdated();
	}

	@Override
	public void onLinkedStorageLayoutChanged() {
		onContentsNbtUpdated();
	}

	@Override
	public void onVirtualCarrierChanged(CompoundTag virtualCarrier) {
		RegistryAccess registryAccess = RegistryHelper.getRegistryAccess()
				.orElseThrow(() -> new IllegalStateException("Registry access is required for linked backpack host state"));
		replaceBackpackStack(ItemStack.parse(registryAccess, virtualCarrier).orElseThrow());
		configureRenderInfo();
		onContentsNbtUpdated();
	}

	@Override
	public Optional<CompoundTag> getVirtualCarrierSnapshot() {
		RegistryAccess registryAccess = RegistryHelper.getRegistryAccess()
				.orElseThrow(() -> new IllegalStateException("Registry access is required for linked backpack host state"));
		return Optional.of((CompoundTag) getBackpack().save(registryAccess));
	}

	@Override
	public Optional<Component> getLinkedStorageDisplayName() {
		return Optional.of(getDisplayName());
	}

	@Override
	public Optional<JukeboxPlaybackLocation> getJukeboxPlaybackLocation(ServerLevel initiatingLevel) {
		return LinkedStorageJukeboxPlaybackAnchors.getPlaybackLocation(initiatingLevel, contents.groupId());
	}

	private static class LinkedStorageBackpackContentsSource implements IBackpackContentsSource {
		private final ILinkedStorageContentsBinding contents;

		private LinkedStorageBackpackContentsSource(ILinkedStorageContentsBinding contents) {
			this.contents = contents;
		}

		@Override
		public CompoundTag getContents() {
			return contents.contents();
		}

		@Override
		public void setContents(CompoundTag contents) {
			this.contents.setContents(this.contents.groupId(), contents);
		}

		@Override
		public void markDirty() {
			contents.markDirty();
		}

		@Override
		public Optional<UUID> getContentsUuid() {
			return Optional.of(contents.groupId());
		}

		@Override
		public boolean usesCanonicalSlotNumbers() {
			return true;
		}
	}
}
