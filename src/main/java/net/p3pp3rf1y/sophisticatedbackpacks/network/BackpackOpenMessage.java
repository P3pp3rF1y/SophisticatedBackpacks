package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.BackpackOpenHandler;

import java.util.function.Supplier;

public class BackpackOpenMessage {
	private BackpackOpenMessage() {}

	public static BackpackOpenMessage getInstance() {
		//noinspection InstantiationOfUtilityClass
		return new BackpackOpenMessage();
	}

	static void onMessage(Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(ServerPlayerEntity player) {
		BackpackOpenHandler.handle(player);
	}
}
