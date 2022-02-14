package net.p3pp3rf1y.sophisticatedcore.compat.jei;

import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;

public class JeiCompat implements ICompat {
	@Override
	public void setup() {
		SophisticatedCore.PACKET_HANDLER.registerMessage(TransferRecipeMessage.class, TransferRecipeMessage::encode, TransferRecipeMessage::decode, TransferRecipeMessage::onMessage);
		SophisticatedCore.PACKET_HANDLER.registerMessage(SetGhostSlotMessage.class, SetGhostSlotMessage::encode, SetGhostSlotMessage::decode, SetGhostSlotMessage::onMessage);
	}
}
