package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ILinkedStorageVirtualHost;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageSnapshotProfile;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.IJukeboxPlaybackLocationProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Optional;
import java.util.UUID;

public class BackpackLinkedStorageHostWrapper extends BackpackWrapper implements ILinkedStorageVirtualHost, IJukeboxPlaybackLocationProvider {
	public static final ResourceLocation FACTORY_ID = SophisticatedBackpacks.getRL("backpack");
	private final ILinkedStorageContents contents;

	public static BackpackLinkedStorageHostWrapper create(ILinkedStorageContents contents, CompoundTag virtualCarrier) {
		return new BackpackLinkedStorageHostWrapper(contents, ItemStack.of(virtualCarrier));
	}

	public BackpackLinkedStorageHostWrapper(ILinkedStorageContents contents, ItemStack virtualCarrier) {
		super(requireBackpack(virtualCarrier), new ContentsSource(contents));
		this.contents = contents;
		getRenderInfo().setRenderUpdateChangeListener(renderInfo -> contents.markRenderDirty());
	}

	public boolean synchronizeEndpointRenderInfo(CompoundTag renderInfo) {
		if (getRenderInfo().getNbt().equals(renderInfo)) {
			return false;
		}

		NBTHelper.setCompoundNBT(getBackpack(), BackpackRenderInfo.RENDER_INFO_TAG, renderInfo.copy());
		replaceBackpackStack(getBackpack());
		getRenderInfo().setRenderUpdateChangeListener(updatedRenderInfo -> contents.markRenderDirty());
		contents.markRenderDirty();
		return true;
	}

	private static ItemStack requireBackpack(ItemStack stack) {
		if (!(stack.getItem() instanceof BackpackItem)) {
			throw new IllegalArgumentException("Linked storage virtual carrier must be a Backpack");
		}
		return stack;
	}

	@Override
	public void setContentsUuid(UUID storageUuid) {
		// The group binding, rather than its virtual carrier, owns this identity.
	}

	@Override
	public void removeContentsUuid() {
		// A linked host never has an ordinary BackpackStorage entry.
	}

	@Override
	public void removeContentsUUIDTag() {
		// The virtual carrier deliberately has no local contents identity.
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
	public void onLinkedStorageContentsChanged() {
		onContentsNbtUpdated();
	}

	@Override
	public void onLinkedStorageLayoutChanged() {
		onContentsNbtUpdated();
	}

	@Override
	public void onVirtualCarrierChanged(CompoundTag virtualCarrier) {
		replaceBackpackStack(requireBackpack(ItemStack.of(virtualCarrier)));
		getRenderInfo().setRenderUpdateChangeListener(renderInfo -> contents.markRenderDirty());
		onContentsNbtUpdated();
	}

	@Override
	public Optional<CompoundTag> getVirtualCarrierSnapshot() {
		return Optional.of(getBackpack().save(new CompoundTag()));
	}

	@Override
	public Optional<Component> getLinkedStorageDisplayName() {
		return Optional.of(getDisplayName());
	}

	@Override
	public Optional<LinkedStorageSnapshotProfile> getLinkedStorageSnapshotProfile() {
		return Optional
				.of(new LinkedStorageSnapshotProfile(getDisplayName(), getInventoryHandler().getSlots(), getUpgradeHandler().getSlots(), getColumnsTaken()));
	}

	@Override
	public Optional<JukeboxPlaybackLocation> getJukeboxPlaybackLocation(ServerLevel initiatingLevel) {
		return LinkedStorageJukeboxPlaybackAnchors.getPlaybackLocation(initiatingLevel, contents.groupId());
	}

	private record ContentsSource(ILinkedStorageContents contents) implements IBackpackContentsSource {
		@Override
		public CompoundTag getContents() {
			return contents.getContents();
		}

		@Override
		public void setContents(CompoundTag contents) {
			this.contents.setContents(contents);
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
