package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.Direction;
import net.minecraft.util.Hand;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryInteractionHelper;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class InventoryInteractionMessage {
	private final BlockPos pos;
	private final Direction face;

	public InventoryInteractionMessage(BlockPos pos, Direction face) {
		this.pos = pos;
		this.face = face;
	}

	public static void encode(InventoryInteractionMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeLong(msg.pos.asLong());
		packetBuffer.writeEnum(msg.face);
	}

	public static InventoryInteractionMessage decode(PacketBuffer packetBuffer) {
		return new InventoryInteractionMessage(BlockPos.of(packetBuffer.readLong()), packetBuffer.readEnum(Direction.class));
	}

	static void onMessage(InventoryInteractionMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg, context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(InventoryInteractionMessage msg, @Nullable ServerPlayerEntity sender) {
		if (sender == null) {
			return;
		}
		SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(sender, (backpack, inventoryName, slot) -> {
			InventoryInteractionHelper.tryInventoryInteraction(msg.pos, sender.level, backpack, msg.face, sender);
			sender.swing(Hand.MAIN_HAND, true);
			return true;
		});
	}
}
