package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import com.mojang.serialization.Codec;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

public class MobCatcherContentsData implements ContainerContents.ISettingsCategoryData<MobCatcherContentsData> {
	public static final Codec<MobCatcherContentsData> CODEC = CompoundTag.CODEC.xmap(MobCatcherContentsData::new, MobCatcherContentsData::data);
	@SuppressWarnings({"unchecked", "rawtypes"})
	public static final StreamCodec<RegistryFriendlyByteBuf, MobCatcherContentsData> STREAM_CODEC = ((StreamCodec<RegistryFriendlyByteBuf, CompoundTag>) (StreamCodec) ByteBufCodecs.COMPOUND_TAG)
			.map(MobCatcherContentsData::new, MobCatcherContentsData::data);

	private CompoundTag data;

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
	}

	public CompoundTag data() {
		return data;
	}
}
