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

public class SyncRenderInfoMessage {
	private final int slotNumber;
	@Nullable
	private final CompoundNBT renderInfoNbt;

	public SyncRenderInfoMessage(int slotNumber, @Nullable CompoundNBT renderInfoNbt) {
		this.slotNumber = slotNumber;
		this.renderInfoNbt = renderInfoNbt;
	}

	public static void encode(SyncRenderInfoMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.slotNumber);
		packetBuffer.writeCompoundTag(msg.renderInfoNbt);
	}

	public static SyncRenderInfoMessage decode(PacketBuffer packetBuffer) {
		return new SyncRenderInfoMessage(packetBuffer.readInt(), packetBuffer.readCompoundTag());
	}

	static void onMessage(SyncRenderInfoMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncRenderInfoMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || msg.renderInfoNbt == null || !(player.openContainer instanceof BackpackContainer)) {
			return;
		}
		ItemStack backpack = player.openContainer.getSlot(msg.slotNumber).getStack();
		backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(backpackWrapper -> backpackWrapper.getRenderInfo().deserializeFrom(msg.renderInfoNbt));
	}
}
