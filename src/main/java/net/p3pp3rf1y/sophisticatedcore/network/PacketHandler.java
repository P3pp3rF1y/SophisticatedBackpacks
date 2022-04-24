package net.p3pp3rf1y.sophisticatedcore.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.network.NetworkDirection;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkRegistry;
import net.minecraftforge.network.simple.SimpleChannel;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.PlayDiscMessage;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.SoundStopNotificationMessage;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.StopDiscPlaybackMessage;
import net.p3pp3rf1y.sophisticatedcore.upgrades.tank.TankClickMessage;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.Supplier;

public class PacketHandler {
	private static final String PROTOCOL = "1";

	private final SimpleChannel networkWrapper;
	private int idx = 0;

	public PacketHandler(String modId) {
		networkWrapper = NetworkRegistry.newSimpleChannel(new ResourceLocation(modId, "channel"),
				() -> PROTOCOL, PROTOCOL::equals, PROTOCOL::equals);
	}

	public void init() {
		registerMessage(SyncContainerClientDataMessage.class, SyncContainerClientDataMessage::encode, SyncContainerClientDataMessage::decode, SyncContainerClientDataMessage::onMessage);
		registerMessage(TransferFullSlotMessage.class, TransferFullSlotMessage::encode, TransferFullSlotMessage::decode, TransferFullSlotMessage::onMessage);
		registerMessage(SyncContainerStacksMessage.class, SyncContainerStacksMessage::encode, SyncContainerStacksMessage::decode, SyncContainerStacksMessage::onMessage);
		registerMessage(SyncSlotStackMessage.class, SyncSlotStackMessage::encode, SyncSlotStackMessage::decode, SyncSlotStackMessage::onMessage);
		registerMessage(SyncPlayerSettingsMessage.class, SyncPlayerSettingsMessage::encode, SyncPlayerSettingsMessage::decode, SyncPlayerSettingsMessage::onMessage);
		registerMessage(PlayDiscMessage.class, PlayDiscMessage::encode, PlayDiscMessage::decode, PlayDiscMessage::onMessage);
		registerMessage(StopDiscPlaybackMessage.class, StopDiscPlaybackMessage::encode, StopDiscPlaybackMessage::decode, StopDiscPlaybackMessage::onMessage);
		registerMessage(SoundStopNotificationMessage.class, SoundStopNotificationMessage::encode, SoundStopNotificationMessage::decode, SoundStopNotificationMessage::onMessage);
		registerMessage(TankClickMessage.class, TankClickMessage::encode, TankClickMessage::decode, TankClickMessage::onMessage);
		registerMessage(StorageInsertMessage.class, StorageInsertMessage::encode, StorageInsertMessage::decode, StorageInsertMessage::onMessage);
	}

	@SuppressWarnings("SameParameterValue")
	public <M> void registerMessage(Class<M> messageType, BiConsumer<M, FriendlyByteBuf> encoder, Function<FriendlyByteBuf, M> decoder, BiConsumer<M, Supplier<NetworkEvent.Context>> messageConsumer) {
		networkWrapper.registerMessage(idx++, messageType, encoder, decoder, messageConsumer);
	}

	public <M> void sendToServer(M message) {
		networkWrapper.sendToServer(message);
	}

	public <M> void sendToClient(ServerPlayer player, M message) {
		networkWrapper.sendTo(message, player.connection.getConnection(), NetworkDirection.PLAY_TO_CLIENT);
	}

	public <M> void sendToAllNear(ServerLevel world, ResourceKey<Level> dimension, Vec3 position, int range, M message) {
		world.players().forEach(player -> {
			if (player.level.dimension() == dimension && player.distanceToSqr(position) <= range * range) {
				sendToClient(player, message);
			}
		});
	}
}
