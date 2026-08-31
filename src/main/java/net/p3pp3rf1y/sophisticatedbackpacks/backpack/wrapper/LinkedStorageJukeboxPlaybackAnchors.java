package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceKey;
import net.minecraft.server.MinecraftServer;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlockEntity;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxPlaybackLocation;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.UUID;

public class LinkedStorageJukeboxPlaybackAnchors {
	private static final int PLAYER_ANCHOR_GRACE_TICKS = 20;
	private static final Map<UUID, Anchor> ANCHORS = new HashMap<>();

	private LinkedStorageJukeboxPlaybackAnchors() {
	}

	public static void refreshBlockAnchor(ServerLevel level, BlockPos pos, ItemStack stack) {
		getPrimaryEndpoint(level, stack).ifPresent(endpoint -> ANCHORS.put(endpoint.groupId(), new BlockAnchor(level.dimension(), pos, endpoint.endpointId())));
	}

	public static void removeBlockAnchor(ServerLevel level, BlockPos pos, ItemStack stack) {
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		if (endpoint != null) {
			ANCHORS.computeIfPresent(endpoint.groupId(),
					(groupId, anchor) -> anchor instanceof BlockAnchor(ResourceKey<Level> dimension, BlockPos anchorPos, UUID endpointId)
							&& dimension.equals(level.dimension()) && anchorPos.equals(pos) && endpointId.equals(endpoint.endpointId()) ? null : anchor);
		}
	}

	public static void refreshPlayerAnchor(ServerPlayer player, ItemStack stack) {
		getPrimaryEndpoint(player.serverLevel(), stack)
				.ifPresent(endpoint -> ANCHORS.put(endpoint.groupId(), new PlayerAnchor(player.getUUID(), player.server.getTickCount())));
	}

	public static Optional<JukeboxPlaybackLocation> getPlaybackLocation(ServerLevel initiatingLevel, UUID groupId) {
		Anchor anchor = ANCHORS.get(groupId);
		if (anchor == null) {
			return Optional.empty();
		}

		Optional<JukeboxPlaybackLocation> location = anchor.resolve(initiatingLevel.getServer(), groupId);
		if (location.isEmpty()) {
			ANCHORS.remove(groupId, anchor);
		}
		return location;
	}

	public static void clear() {
		ANCHORS.clear();
	}

	public static boolean isPrimaryEndpoint(ServerLevel level, ItemStack stack) {
		return getPrimaryEndpoint(level, stack).isPresent();
	}

	private static Optional<LinkedStorageEndpointData> getPrimaryEndpoint(ServerLevel level, ItemStack stack) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(stack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return Optional.empty();
		}
		LinkedStorageEndpointData endpoint = stack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		return LinkedStorageGroupsSavedData.get(level).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())
				? Optional.of(endpoint)
				: Optional.empty();
	}

	private sealed interface Anchor permits BlockAnchor, PlayerAnchor {
		Optional<JukeboxPlaybackLocation> resolve(MinecraftServer server, UUID groupId);
	}

	private record BlockAnchor(ResourceKey<Level> dimension, BlockPos pos, UUID endpointId) implements Anchor {
		@Override
		public Optional<JukeboxPlaybackLocation> resolve(MinecraftServer server, UUID groupId) {
			ServerLevel level = server.getLevel(dimension);
			if (level == null || !level.hasChunkAt(pos) || !(level.getBlockEntity(pos) instanceof BackpackBlockEntity backpack)) {
				return Optional.empty();
			}
			LinkedStorageEndpointData endpoint = backpack.getLinkedStorageEndpointData();
			return endpoint != null && endpoint.groupId().equals(groupId) && endpoint.endpointId().equals(endpointId)
					? Optional.of(JukeboxPlaybackLocation.forBlock(level, pos))
					: Optional.empty();
		}
	}

	private record PlayerAnchor(UUID playerId, int lastSeenTick) implements Anchor {
		@Override
		public Optional<JukeboxPlaybackLocation> resolve(MinecraftServer server, UUID groupId) {
			if (lastSeenTick < server.getTickCount() - PLAYER_ANCHOR_GRACE_TICKS) {
				return Optional.empty();
			}
			ServerPlayer player = server.getPlayerList().getPlayer(playerId);
			return player == null ? Optional.empty() : Optional.of(JukeboxPlaybackLocation.forEntity(player));
		}
	}
}
