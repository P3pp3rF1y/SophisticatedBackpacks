package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraftforge.network.NetworkDirection;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;

public class SBPPacketHandler extends PacketHandler {
	public static final SBPPacketHandler INSTANCE = new SBPPacketHandler(SophisticatedBackpacks.MOD_ID, SophisticatedBackpacks.getNetworkProtocolVersion());

	private SBPPacketHandler(String modId, String protocol) {
		super(modId, protocol);
	}

	@SuppressWarnings({"java:S2440", "InstantiationOfUtilityClass"})
	@Override
	public void registerMessages() {
		registerMessage(BackpackOpenMessage.class, BackpackOpenMessage::encode, BackpackOpenMessage::decode, BackpackOpenMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(UpgradeToggleMessage.class, UpgradeToggleMessage::encode, UpgradeToggleMessage::decode, UpgradeToggleMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(RequestBackpackInventoryContentsMessage.class, RequestBackpackInventoryContentsMessage::encode,
				RequestBackpackInventoryContentsMessage::decode, RequestBackpackInventoryContentsMessage::onMessage, NetworkDirection.PLAY_TO_SERVER);
		registerMessage(BackpackContentsMessage.class, BackpackContentsMessage::encode, BackpackContentsMessage::decode, BackpackContentsMessage::onMessage,
				NetworkDirection.PLAY_TO_CLIENT);
		registerMessage(InventoryInteractionMessage.class, InventoryInteractionMessage::encode, InventoryInteractionMessage::decode,
				InventoryInteractionMessage::onMessage, NetworkDirection.PLAY_TO_SERVER);
		registerMessage(BlockToolSwapMessage.class, BlockToolSwapMessage::encode, BlockToolSwapMessage::decode, BlockToolSwapMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(EntityToolSwapMessage.class, EntityToolSwapMessage::encode, EntityToolSwapMessage::decode, EntityToolSwapMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(BackpackCloseMessage.class, (backpackCloseMessage, packetBuffer) -> {
		}, packetBuffer -> new BackpackCloseMessage(), (backpackCloseMessage, contextSupplier) -> BackpackCloseMessage.onMessage(contextSupplier),
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(SyncClientInfoMessage.class, SyncClientInfoMessage::encode, SyncClientInfoMessage::decode, SyncClientInfoMessage::onMessage,
				NetworkDirection.PLAY_TO_CLIENT);
		registerMessage(AnotherPlayerBackpackOpenMessage.class, AnotherPlayerBackpackOpenMessage::encode, AnotherPlayerBackpackOpenMessage::decode,
				AnotherPlayerBackpackOpenMessage::onMessage, NetworkDirection.PLAY_TO_SERVER);
		registerMessage(BlockPickMessage.class, BlockPickMessage::encode, BlockPickMessage::decode, BlockPickMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
		registerMessage(MobCatcherCaptureEffectMessage.class, MobCatcherCaptureEffectMessage::encode, MobCatcherCaptureEffectMessage::decode,
				MobCatcherCaptureEffectMessage::onMessage, NetworkDirection.PLAY_TO_CLIENT);
		registerMessage(MobCatcherReleaseMessage.class, MobCatcherReleaseMessage::encode, MobCatcherReleaseMessage::decode, MobCatcherReleaseMessage::onMessage,
				NetworkDirection.PLAY_TO_SERVER);
	}
}
