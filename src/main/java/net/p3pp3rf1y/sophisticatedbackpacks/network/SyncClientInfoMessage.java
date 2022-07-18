package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class SyncClientInfoMessage {
	private final int slotIndex;
	@Nullable
	private final CompoundTag renderInfoNbt;
	private final int columnsTaken;

	public SyncClientInfoMessage(int slotNumber, @Nullable CompoundTag renderInfoNbt, int columnsTaken) {
		slotIndex = slotNumber;
		this.renderInfoNbt = renderInfoNbt;
		this.columnsTaken = columnsTaken;
	}

	public static void encode(SyncClientInfoMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
		packetBuffer.writeNbt(msg.renderInfoNbt);
		packetBuffer.writeInt(msg.columnsTaken);
	}

	public static SyncClientInfoMessage decode(FriendlyByteBuf packetBuffer) {
		return new SyncClientInfoMessage(packetBuffer.readInt(), packetBuffer.readNbt(), packetBuffer.readInt());
	}

	static void onMessage(SyncClientInfoMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncClientInfoMessage msg) {
		LocalPlayer player = Minecraft.getInstance().player;
		if (player == null || msg.renderInfoNbt == null || !(player.containerMenu instanceof BackpackContainer)) {
			return;
		}
		ItemStack backpack = player.getInventory().items.get(msg.slotIndex);
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
			backpackWrapper.getRenderInfo().deserializeFrom(msg.renderInfoNbt);
			backpackWrapper.setColumnsTaken(msg.columnsTaken, false);
		});
	}
}
