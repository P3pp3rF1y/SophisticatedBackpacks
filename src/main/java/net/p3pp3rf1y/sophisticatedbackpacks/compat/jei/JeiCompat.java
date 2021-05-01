package net.p3pp3rf1y.sophisticatedbackpacks.compat.jei;

import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

public class JeiCompat implements ICompat {
	@Override
	public void setup() {
		PacketHandler.registerMessage(TransferRecipeMessage.class, TransferRecipeMessage::encode, TransferRecipeMessage::decode, TransferRecipeMessage::onMessage);
		PacketHandler.registerMessage(SetGhostSlotMessage.class, SetGhostSlotMessage::encode, SetGhostSlotMessage::decode, SetGhostSlotMessage::onMessage);
	}
}
