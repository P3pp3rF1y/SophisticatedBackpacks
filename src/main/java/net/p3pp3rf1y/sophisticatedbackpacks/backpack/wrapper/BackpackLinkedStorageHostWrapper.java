package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.NbtOps;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.Identifier;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContentsBinding;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.IJukeboxPlaybackLocationProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;

import java.util.Optional;
import java.util.UUID;

public class BackpackLinkedStorageHostWrapper extends BackpackWrapper implements ILinkedStorageVirtualHost, IJukeboxPlaybackLocationProvider {
	public static final Identifier FACTORY_ID = SophisticatedBackpacks.getIdentifier("backpack");
	private final ILinkedStorageContentsBinding contents;

	public static BackpackLinkedStorageHostWrapper create(ILinkedStorageContentsBinding contents, CompoundTag virtualCarrier) {
		return new BackpackLinkedStorageHostWrapper(contents, ItemStack.CODEC.parse(NbtOps.INSTANCE, virtualCarrier).getOrThrow());
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
		getRenderDataHandler().setRenderUpdateChangeListener(renderData -> contents.markRenderDirty());
	}

	public boolean synchronizeEndpointRenderData(RenderData renderData) {
		if (getRenderDataHandler().getData().equals(renderData)) {
			return false;
		}
		getBackpack().set(ModCoreDataComponents.RENDER_DATA, renderData.copy());
		replaceBackpackStack(getBackpack());
		configureRenderInfo();
		contents.markRenderDirty();
		return true;
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
		onContentsUpdated();
	}

	@Override
	public void onLinkedStorageLayoutChanged() {
		onContentsUpdated();
	}

	@Override
	public void onVirtualCarrierChanged(CompoundTag virtualCarrier) {
		replaceBackpackStack(ItemStack.CODEC.parse(NbtOps.INSTANCE, virtualCarrier).getOrThrow());
		configureRenderInfo();
		onContentsUpdated();
	}

	@Override
	public Optional<CompoundTag> getVirtualCarrierSnapshot() {
		return Optional.of((CompoundTag) ItemStack.CODEC.encodeStart(NbtOps.INSTANCE, getBackpack()).getOrThrow());
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
		public ContainerContents getContents() {
			return contents.contents();
		}

		@Override
		public void setContents(ContainerContents contents) {
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
