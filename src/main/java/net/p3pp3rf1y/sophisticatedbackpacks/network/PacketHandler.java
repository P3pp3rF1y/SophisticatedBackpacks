package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.RegistryKey;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.math.vector.Vector3d;
import net.minecraft.world.World;
import net.minecraft.world.server.ServerWorld;
import net.minecraftforge.fml.network.NetworkDirection;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.fml.network.NetworkRegistry;
import net.minecraftforge.fml.network.simple.SimpleChannel;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.PlayDiscMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.SoundStopNotificationMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.StopDiscPlaybackMessage;

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
		registerMessage(RequestBackpackContentsMessage.class, RequestBackpackContentsMessage::encode, RequestBackpackContentsMessage::decode, RequestBackpackContentsMessage::onMessage);
		registerMessage(BackpackContentsMessage.class, BackpackContentsMessage::encode, BackpackContentsMessage::decode, BackpackContentsMessage::onMessage);
		registerMessage(InventoryInteractionMessage.class, InventoryInteractionMessage::encode, InventoryInteractionMessage::decode, InventoryInteractionMessage::onMessage);
		registerMessage(TransferFullSlotMessage.class, TransferFullSlotMessage::encode, TransferFullSlotMessage::decode, TransferFullSlotMessage::onMessage);
		registerMessage(SyncContainerStacksMessage.class, SyncContainerStacksMessage::encode, SyncContainerStacksMessage::decode, SyncContainerStacksMessage::onMessage);
		registerMessage(SyncSlotStackMessage.class, SyncSlotStackMessage::encode, SyncSlotStackMessage::decode, SyncSlotStackMessage::onMessage);
		registerMessage(WindowClickMessage.class, WindowClickMessage::encode, WindowClickMessage::decode, WindowClickMessage::onMessage);
		registerMessage(PlayDiscMessage.class, PlayDiscMessage::encode, PlayDiscMessage::decode, PlayDiscMessage::onMessage);
		registerMessage(StopDiscPlaybackMessage.class, StopDiscPlaybackMessage::encode, StopDiscPlaybackMessage::decode, StopDiscPlaybackMessage::onMessage);
		registerMessage(SoundStopNotificationMessage.class, SoundStopNotificationMessage::encode, SoundStopNotificationMessage::decode, SoundStopNotificationMessage::onMessage);
	}

	@SuppressWarnings("SameParameterValue")
	public static <M> void registerMessage(Class<M> messageType, BiConsumer<M, PacketBuffer> encoder, Function<PacketBuffer, M> decoder, BiConsumer<M, Supplier<NetworkEvent.Context>> messageConsumer) {
		networkWrapper.registerMessage(idx++, messageType, encoder, decoder, messageConsumer);
	}

	public static <M> void sendToServer(M message) {
		networkWrapper.sendToServer(message);
	}

	public static <M> void sendToClient(ServerPlayerEntity player, M message) {
		networkWrapper.sendTo(message, player.connection.getNetworkManager(), NetworkDirection.PLAY_TO_CLIENT);
	}

	public static <M> void sendToAllNear(ServerWorld world, RegistryKey<World> dimension, Vector3d position, int range, M message) {
		world.getPlayers().forEach(player -> {
			if (player.world.getDimensionKey() == dimension && player.getDistanceSq(position) <= range * range) {
				sendToClient(player, message);
			}
		});
	}
}
