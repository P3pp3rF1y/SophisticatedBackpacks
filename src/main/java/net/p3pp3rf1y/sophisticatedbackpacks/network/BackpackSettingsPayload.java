package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.core.UUIDUtil;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

import java.util.UUID;

public record BackpackSettingsPayload(UUID backpackUuid,
									  ContainerContents.SettingsData settingsData,
									  CompoundTag additionalContents,
									  boolean includesAdditionalContents) implements CustomPacketPayload {
	public BackpackSettingsPayload(UUID backpackUuid, ContainerContents.SettingsData settingsData) {
		this(backpackUuid, settingsData, new CompoundTag(), false);
	}

	public BackpackSettingsPayload(UUID backpackUuid, ContainerContents.SettingsData settingsData, CompoundTag additionalContents) {
		this(backpackUuid, settingsData, additionalContents, true);
	}

	public static final Type<BackpackSettingsPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("backpack_settings"));
	public static final StreamCodec<RegistryFriendlyByteBuf, BackpackSettingsPayload> STREAM_CODEC = StreamCodec.composite(
			UUIDUtil.STREAM_CODEC,
			BackpackSettingsPayload::backpackUuid,
			ContainerContents.SettingsData.STREAM_CODEC,
			BackpackSettingsPayload::settingsData,
			ByteBufCodecs.COMPOUND_TAG,
			BackpackSettingsPayload::additionalContents,
			ByteBufCodecs.BOOL,
			BackpackSettingsPayload::includesAdditionalContents,
			BackpackSettingsPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(BackpackSettingsPayload payload, IPayloadContext context) {
		if (payload.settingsData == null) {
			return;
		}

		BackpackStorage backpackStorage = BackpackStorage.get();
		ContainerContents contents = backpackStorage.getOrCreateBackpackContents(payload.backpackUuid);
		backpackStorage.setBackpackContents(payload.backpackUuid, new ContainerContents(contents.inventory(), contents.partitioner(), contents.upgrades(), payload.settingsData));
		if (payload.includesAdditionalContents) {
			backpackStorage.setAdditionalContents(payload.backpackUuid, payload.additionalContents);
		}
		ClientStorageContentsTooltipBase.refreshContents();
	}
}
