package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;

public class SBPPacketHandler extends PacketHandler {
	public static final SBPPacketHandler INSTANCE = new SBPPacketHandler(SophisticatedBackpacks.MOD_ID);

	public SBPPacketHandler(String modId) {
		super(modId);
	}

	@SuppressWarnings({"java:S2440", "InstantiationOfUtilityClass"})
	@Override
	public void init() {
		registerMessage(BackpackOpenMessage.class, BackpackOpenMessage::encode, BackpackOpenMessage::decode, BackpackOpenMessage::onMessage);
		registerMessage(UpgradeToggleMessage.class, UpgradeToggleMessage::encode, UpgradeToggleMessage::decode, UpgradeToggleMessage::onMessage);
		registerMessage(RequestBackpackInventoryContentsMessage.class, RequestBackpackInventoryContentsMessage::encode, RequestBackpackInventoryContentsMessage::decode, RequestBackpackInventoryContentsMessage::onMessage);
		registerMessage(BackpackContentsMessage.class, BackpackContentsMessage::encode, BackpackContentsMessage::decode, BackpackContentsMessage::onMessage);
		registerMessage(InventoryInteractionMessage.class, InventoryInteractionMessage::encode, InventoryInteractionMessage::decode, InventoryInteractionMessage::onMessage);
		registerMessage(BlockToolSwapMessage.class, BlockToolSwapMessage::encode, BlockToolSwapMessage::decode, BlockToolSwapMessage::onMessage);
		registerMessage(EntityToolSwapMessage.class, EntityToolSwapMessage::encode, EntityToolSwapMessage::decode, EntityToolSwapMessage::onMessage);
		registerMessage(BackpackCloseMessage.class, (backpackCloseMessage, packetBuffer) -> {}, packetBuffer -> new BackpackCloseMessage(), (backpackCloseMessage, contextSupplier) -> BackpackCloseMessage.onMessage(contextSupplier));
		registerMessage(SyncClientInfoMessage.class, SyncClientInfoMessage::encode, SyncClientInfoMessage::decode, SyncClientInfoMessage::onMessage);
		registerMessage(AnotherPlayerBackpackOpenMessage.class, AnotherPlayerBackpackOpenMessage::encode, AnotherPlayerBackpackOpenMessage::decode, AnotherPlayerBackpackOpenMessage::onMessage);
		registerMessage(BlockPickMessage.class, BlockPickMessage::encode, BlockPickMessage::decode, BlockPickMessage::onMessage);
	}
}
