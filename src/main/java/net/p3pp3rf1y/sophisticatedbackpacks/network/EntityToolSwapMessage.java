package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.Entity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraft.world.World;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IEntityToolSwapUpgrade;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

public class EntityToolSwapMessage {
	private final int entityId;

	public EntityToolSwapMessage(int entityId) {
		this.entityId = entityId;
	}

	public static void encode(EntityToolSwapMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.entityId);
	}

	public static EntityToolSwapMessage decode(PacketBuffer packetBuffer) {
		return new EntityToolSwapMessage(packetBuffer.readInt());
	}

	static void onMessage(EntityToolSwapMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg, context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(EntityToolSwapMessage msg, @Nullable ServerPlayerEntity sender) {
		if (sender == null) {
			return;
		}

		World world = sender.level;
		Entity entity = world.getEntity(msg.entityId);

		if (entity == null) {
			return;
		}

		AtomicBoolean result = new AtomicBoolean(false);
		AtomicBoolean anyUpgradeCanInteract = new AtomicBoolean(false);
		SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(sender, (backpack, inventoryName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(backpackWrapper -> {
					backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IEntityToolSwapUpgrade.class)
							.forEach(upgrade -> {
								if (!upgrade.canProcessEntityInteract() || result.get()) {
									return;
								}
								anyUpgradeCanInteract.set(true);

								result.set(upgrade.onEntityInteract(world, entity, sender));
							});
							return result.get();
						}
				).orElse(false)
		);

		if (!anyUpgradeCanInteract.get()) {
			sender.displayClientMessage(new TranslationTextComponent("gui.sophisticatedbackpacks.status.no_tool_swap_upgrade_present"), true);
			return;
		}
		if (!result.get()) {
			sender.displayClientMessage(new TranslationTextComponent("gui.sophisticatedbackpacks.status.no_tool_found_for_entity"), true);
		}
	}
}
