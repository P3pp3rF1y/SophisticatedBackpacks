package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;

public class JeiCompat implements ICompat {
	@Override
	public void setup() {
		SophisticatedBackpacks.PACKET_HANDLER.registerMessage(TransferRecipeMessage.class, TransferRecipeMessage::encode, TransferRecipeMessage::decode, TransferRecipeMessage::onMessage);
		SophisticatedBackpacks.PACKET_HANDLER.registerMessage(SetGhostSlotMessage.class, SetGhostSlotMessage::encode, SetGhostSlotMessage::decode, SetGhostSlotMessage::onMessage);
	}
}
