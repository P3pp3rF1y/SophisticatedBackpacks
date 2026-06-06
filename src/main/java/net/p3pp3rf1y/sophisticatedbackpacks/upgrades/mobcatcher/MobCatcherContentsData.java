package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import com.mojang.serialization.Codec;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

import java.util.List;
import java.util.Objects;

public class MobCatcherContentsData implements ContainerContents.ISettingsCategoryData<MobCatcherContentsData> {
	public static final Codec<MobCatcherContentsData> CODEC = CompoundTag.CODEC.xmap(MobCatcherContentsData::new, MobCatcherContentsData::data);
	@SuppressWarnings({"unchecked", "rawtypes"})
	public static final StreamCodec<RegistryFriendlyByteBuf, MobCatcherContentsData> STREAM_CODEC = ((StreamCodec<RegistryFriendlyByteBuf, CompoundTag>) (StreamCodec) ByteBufCodecs.COMPOUND_TAG)
			.map(MobCatcherContentsData::new, MobCatcherContentsData::data);

	private CompoundTag data;
	private List<CapturedMob> capturedMobs;

	public MobCatcherContentsData() {
		this(new CompoundTag());
	}

	public MobCatcherContentsData(CompoundTag data) {
		this.data = data.copy();
	}

	@Override
	public String id() {
		return MobCatcherStorage.CAPTURED_MOBS_TAG;
	}

	@Override
	public MobCatcherContentsData copy() {
		return new MobCatcherContentsData(data);
	}

	@Override
	public void reloadFrom(MobCatcherContentsData other) {
		data = other.data.copy();
		capturedMobs = null;
	}

	public CompoundTag data() {
		return data.copy();
	}

	public void setData(CompoundTag data) {
		this.data = data.copy();
		capturedMobs = null;
	}

	public List<CapturedMob> getCapturedMobs() {
		if (capturedMobs == null) {
			capturedMobs = MobCatcherStorage.deserializeCapturedMobs(data);
		}
		return capturedMobs;
	}

	public void setCapturedMobs(List<CapturedMob> capturedMobs) {
		this.capturedMobs = List.copyOf(capturedMobs);
		CompoundTag updatedData = data.copy();
		if (capturedMobs.isEmpty()) {
			updatedData.remove(MobCatcherStorage.CAPTURED_MOBS_TAG);
		} else {
			updatedData.put(MobCatcherStorage.CAPTURED_MOBS_TAG, MobCatcherStorage.serialize(capturedMobs));
		}
		data = updatedData;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (!(obj instanceof MobCatcherContentsData that)) {
			return false;
		}
		return Objects.equals(data, that.data);
	}

	@Override
	public int hashCode() {
		return Objects.hash(data);
	}
}
