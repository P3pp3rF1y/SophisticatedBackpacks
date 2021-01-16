package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.PacketBuffer;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.fml.network.NetworkRegistry;
import net.minecraftforge.fml.network.simple.SimpleChannel;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class PacketHandler {
	private PacketHandler() {}

	private static SimpleChannel networkWrapper;
	private static final String PROTOCOL = "1";
	private static int idx = 0;

	public static void init() {
		networkWrapper = NetworkRegistry.newSimpleChannel(new ResourceLocation(SophisticatedBackpacks.MOD_ID, "channel"),
				() -> PROTOCOL, PROTOCOL::equals, PROTOCOL::equals);

		registerMessage(BackpackOpenMessage.class, BackpackOpenMessage::encode, BackpackOpenMessage::decode, BackpackOpenMessage::onMessage);
		registerMessage(ServerBackpackDataMessage.class, ServerBackpackDataMessage::encode, ServerBackpackDataMessage::decode, ServerBackpackDataMessage::onMessage);
		registerMessage(UpgradeToggleMessage.class, UpgradeToggleMessage::encode, UpgradeToggleMessage::decode, UpgradeToggleMessage::onMessage);
	}

	@SuppressWarnings("SameParameterValue")
	private static <M> void registerMessage(Class<M> messageType, BiConsumer<M, PacketBuffer> encoder, Function<PacketBuffer, M> decoder, BiConsumer<M, Supplier<NetworkEvent.Context>> messageConsumer) {
		networkWrapper.registerMessage(idx++, messageType, encoder, decoder, messageConsumer);
	}

	public static <M> void sendToServer(M message) {
		networkWrapper.sendToServer(message);
	}
}
