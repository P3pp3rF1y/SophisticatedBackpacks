package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.core.UUIDUtil;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackStorage;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.inventory.ContainerContents;

import java.util.UUID;

public record BackpackSettingsPayload(UUID backpackUuid,
									  ContainerContents.SettingsData settingsData) implements CustomPacketPayload {
	public static final Type<BackpackSettingsPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("backpack_settings"));
	public static final StreamCodec<RegistryFriendlyByteBuf, BackpackSettingsPayload> STREAM_CODEC = StreamCodec.composite(
			UUIDUtil.STREAM_CODEC,
			BackpackSettingsPayload::backpackUuid,
			ContainerContents.SettingsData.STREAM_CODEC,
			BackpackSettingsPayload::settingsData,
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
		ClientStorageContentsTooltipBase.refreshContents();
	}
}
