package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.Level;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEntityToolSwapUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.concurrent.atomic.AtomicBoolean;

public record EntityToolSwapPayload(int entityId) implements CustomPacketPayload {
	public static final Type<EntityToolSwapPayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("entity_tool_swap"));
	public static final StreamCodec<ByteBuf, EntityToolSwapPayload> STREAM_CODEC = StreamCodec.composite(ByteBufCodecs.INT, EntityToolSwapPayload::entityId,
			EntityToolSwapPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(EntityToolSwapPayload payload, IPayloadContext context) {
		Player player = context.player();
		Level level = player.level();
		Entity entity = level.getEntity(payload.entityId);

		if (entity == null) {
			return;
		}

		AtomicBoolean result = new AtomicBoolean(false);
		AtomicBoolean anyUpgradeCanInteract = new AtomicBoolean(false);
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, identifier, slot) -> {
			BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(level, backpack).getUpgradeHandler()
					.getWrappersThatImplement(IEntityToolSwapUpgrade.class).forEach(upgrade -> {
						if (!upgrade.canProcessEntityInteract() || result.get()) {
							return;
						}
						anyUpgradeCanInteract.set(true);

						result.set(upgrade.onEntityInteract(level, entity, player));
					});
			return result.get();
		});

		if (!anyUpgradeCanInteract.get()) {
			player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.no_tool_swap_upgrade_present"), true);
			return;
		}
		if (!result.get()) {
			player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.no_tool_found_for_entity"), true);
		}
	}
}
