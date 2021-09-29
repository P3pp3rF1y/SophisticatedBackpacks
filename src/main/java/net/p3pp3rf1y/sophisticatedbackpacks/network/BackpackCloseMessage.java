package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.fmllegacy.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

@SuppressWarnings("java:S1118")
public class BackpackCloseMessage {
	static void onMessage(Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player) {
		if (player == null) {
			return;
		}

		if (player.containerMenu instanceof BackpackContainer) {
			player.closeContainer();
		}
	}
}
