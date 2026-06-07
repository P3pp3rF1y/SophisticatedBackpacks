package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.neoforged.neoforge.network.event.RegisterPayloadHandlersEvent;
import net.neoforged.neoforge.network.registration.PayloadRegistrar;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.network.*;

public class ModPayloads {
	private ModPayloads() {
	}

	public static void registerPayloads(final RegisterPayloadHandlersEvent event) {
		final PayloadRegistrar registrar = event.registrar(SophisticatedBackpacks.MOD_ID).versioned(SophisticatedBackpacks.getNetworkProtocolVersion());
		registrar.playToServer(BackpackOpenPayload.TYPE, BackpackOpenPayload.STREAM_CODEC, BackpackOpenPayload::handlePayload);
		registrar.playToServer(UpgradeTogglePayload.TYPE, UpgradeTogglePayload.STREAM_CODEC, UpgradeTogglePayload::handlePayload);
		registrar.playToServer(RequestBackpackInventoryContentsPayload.TYPE, RequestBackpackInventoryContentsPayload.STREAM_CODEC, RequestBackpackInventoryContentsPayload::handlePayload);
		registrar.playToClient(BackpackContentsPayload.TYPE, BackpackContentsPayload.STREAM_CODEC, BackpackContentsPayload::handlePayload);
		registrar.playToServer(InventoryInteractionPayload.TYPE, InventoryInteractionPayload.STREAM_CODEC, InventoryInteractionPayload::handlePayload);
		registrar.playToServer(BlockToolSwapPayload.TYPE, BlockToolSwapPayload.STREAM_CODEC, BlockToolSwapPayload::handlePayload);
		registrar.playToServer(EntityToolSwapPayload.TYPE, EntityToolSwapPayload.STREAM_CODEC, EntityToolSwapPayload::handlePayload);
		registrar.playToServer(BackpackClosePayload.TYPE, BackpackClosePayload.STREAM_CODEC, BackpackClosePayload::handlePayload);
		registrar.playToClient(SyncClientInfoPayload.TYPE, SyncClientInfoPayload.STREAM_CODEC, SyncClientInfoPayload::handlePayload);
		registrar.playToServer(AnotherPlayerBackpackOpenPayload.TYPE, AnotherPlayerBackpackOpenPayload.STREAM_CODEC, AnotherPlayerBackpackOpenPayload::handlePayload);
		registrar.playToServer(BlockPickPayload.TYPE, BlockPickPayload.STREAM_CODEC, BlockPickPayload::handlePayload);
		registrar.playToServer(RequestPlayerSettingsPayload.TYPE, RequestPlayerSettingsPayload.STREAM_CODEC, RequestPlayerSettingsPayload::handlePayload);
		registrar.playToServer(MobCatcherReleasePayload.TYPE, MobCatcherReleasePayload.STREAM_CODEC, MobCatcherReleasePayload::handlePayload);
		registrar.playToClient(MobCatcherCaptureEffectPayload.TYPE, MobCatcherCaptureEffectPayload.STREAM_CODEC, MobCatcherCaptureEffectPayload::handlePayload);
	}
}
