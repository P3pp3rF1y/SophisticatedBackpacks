package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class SyncClientInfoMessage {
	private final int slotIndex;
	@Nullable
	private final CompoundNBT renderInfoNbt;
	private final int columnsTaken;

	public SyncClientInfoMessage(int slotNumber, @Nullable CompoundNBT renderInfoNbt, int columnsTaken) {
		slotIndex = slotNumber;
		this.renderInfoNbt = renderInfoNbt;
		this.columnsTaken = columnsTaken;
	}

	public static void encode(SyncClientInfoMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.slotIndex);
		packetBuffer.writeNbt(msg.renderInfoNbt);
		packetBuffer.writeInt(msg.columnsTaken);
	}

	public static SyncClientInfoMessage decode(PacketBuffer packetBuffer) {
		return new SyncClientInfoMessage(packetBuffer.readInt(), packetBuffer.readNbt(), packetBuffer.readInt());
	}

	static void onMessage(SyncClientInfoMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncClientInfoMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || msg.renderInfoNbt == null || !(player.containerMenu instanceof BackpackContainer)) {
			return;
		}
		ItemStack backpack = player.inventory.items.get(msg.slotIndex);
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> {
			backpackWrapper.getRenderInfo().deserializeFrom(msg.renderInfoNbt);
			backpackWrapper.setColumnsTaken(msg.columnsTaken);
		});
	}
}
