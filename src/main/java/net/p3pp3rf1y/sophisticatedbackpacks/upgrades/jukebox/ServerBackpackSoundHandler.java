package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox;

import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.event.TickEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class ServerBackpackSoundHandler {
	private ServerBackpackSoundHandler() {}

	private static final int KEEP_ALIVE_CHECK_INTERVAL = 10;
	private static final Map<ResourceKey<Level>, Long> lastWorldCheck = new HashMap<>();
	private static final Map<ResourceKey<Level>, Map<UUID, KeepAliveInfo>> worldBackpackKeepAlive = new HashMap<>();

	public static void init() {
		MinecraftForge.EVENT_BUS.addListener(ServerBackpackSoundHandler::tick);
	}

	public static void tick(TickEvent.WorldTickEvent event) {
		if (event.phase != TickEvent.Phase.END || event.world.isClientSide()) {
			return;
		}
		ServerLevel world = (ServerLevel) event.world;
		ResourceKey<Level> dim = world.dimension();
		if (lastWorldCheck.computeIfAbsent(dim, key -> world.getGameTime()) > world.getGameTime() - KEEP_ALIVE_CHECK_INTERVAL || !worldBackpackKeepAlive.containsKey(dim)) {
			return;
		}
		lastWorldCheck.put(dim, world.getGameTime());

		worldBackpackKeepAlive.get(dim).entrySet().removeIf(entry -> {
			if (entry.getValue().getLastKeepAliveTime() < world.getGameTime() - KEEP_ALIVE_CHECK_INTERVAL) {
				sendStopMessage(world, entry.getValue().getLastPosition(), entry.getKey());
				return true;
			}
			return false;
		});
	}

	public static void updateKeepAlive(UUID backpackUuid, Level world, Vec3 position, Runnable onNoLongerRunning) {
		ResourceKey<Level> dim = world.dimension();
		if (!worldBackpackKeepAlive.containsKey(dim) || !worldBackpackKeepAlive.get(dim).containsKey(backpackUuid)) {
			onNoLongerRunning.run();
			return;
		}
		if (worldBackpackKeepAlive.get(dim).containsKey(backpackUuid)) {
			worldBackpackKeepAlive.get(dim).get(backpackUuid).update(world.getGameTime(), position);
		}
	}

	public static void onSoundStopped(ServerLevel world, UUID backpackUuid) {
		removeKeepAliveInfo(world, backpackUuid);
	}

	private static class KeepAliveInfo {
		private final WeakReference<Runnable> onStopHandler;
		private long lastKeepAliveTime;
		private Vec3 lastPosition;

		private KeepAliveInfo(Runnable onStopHandler, long lastKeepAliveTime, Vec3 lastPosition) {
			this.onStopHandler = new WeakReference<>(onStopHandler);
			this.lastKeepAliveTime = lastKeepAliveTime;
			this.lastPosition = lastPosition;
		}

		public long getLastKeepAliveTime() {
			return lastKeepAliveTime;
		}

		public Vec3 getLastPosition() {
			return lastPosition;
		}

		public void update(long gameTime, Vec3 position) {
			lastKeepAliveTime = gameTime;
			lastPosition = position;
		}

		public void runOnStop() {
			Runnable handler = onStopHandler.get();
			if (handler != null) {
				handler.run();
			}
		}
	}

	public static void startPlayingDisc(ServerLevel serverWorld, BlockPos position, UUID backpackUuid, int discItemId, Runnable onStopHandler) {
		Vec3 pos = Vec3.atCenterOf(position);
		PacketHandler.sendToAllNear(serverWorld, serverWorld.dimension(), pos, 128, new PlayDiscMessage(backpackUuid, discItemId, position));
		putKeepAliveInfo(serverWorld, backpackUuid, onStopHandler, pos);
	}

	public static void startPlayingDisc(ServerLevel serverWorld, Vec3 position, UUID backpackUuid, int entityId, int discItemId, Runnable onStopHandler) {
		PacketHandler.sendToAllNear(serverWorld, serverWorld.dimension(), position, 128, new PlayDiscMessage(backpackUuid, discItemId, entityId));
		putKeepAliveInfo(serverWorld, backpackUuid, onStopHandler, position);
	}

	private static void putKeepAliveInfo(ServerLevel serverWorld, UUID backpackUuid, Runnable onStopHandler, Vec3 pos) {
		worldBackpackKeepAlive.computeIfAbsent(serverWorld.dimension(), dim -> new HashMap<>()).put(backpackUuid, new KeepAliveInfo(onStopHandler, serverWorld.getGameTime(), pos));
	}

	public static void stopPlayingDisc(ServerLevel serverWorld, Vec3 position, UUID backpackUuid) {
		removeKeepAliveInfo(serverWorld, backpackUuid);
		sendStopMessage(serverWorld, position, backpackUuid);
	}

	private static void removeKeepAliveInfo(ServerLevel serverWorld, UUID backpackUuid) {
		ResourceKey<Level> dim = serverWorld.dimension();
		if (worldBackpackKeepAlive.containsKey(dim) && worldBackpackKeepAlive.get(dim).containsKey(backpackUuid)) {
			worldBackpackKeepAlive.get(dim).remove(backpackUuid).runOnStop();
		}
	}

	private static void sendStopMessage(ServerLevel serverWorld, Vec3 position, UUID backpackUuid) {
		PacketHandler.sendToAllNear(serverWorld, serverWorld.dimension(), position, 128, new StopDiscPlaybackMessage(backpackUuid));
	}
}
