package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.NonNullList;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import java.util.List;
import java.util.function.Supplier;

public class SyncContainerStacksMessage {
	private final int windowId;
	private final List<ItemStack> itemStacks;

	public SyncContainerStacksMessage(int windowId, List<ItemStack> itemStacks) {
		this.windowId = windowId;
		this.itemStacks = itemStacks;
	}

	public static void encode(SyncContainerStacksMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeByte(msg.windowId);
		packetBuffer.writeShort(msg.itemStacks.size());

		for (ItemStack itemstack : msg.itemStacks) {
			PacketHelper.writeItemStack(itemstack, packetBuffer);
		}
	}

	public static SyncContainerStacksMessage decode(PacketBuffer packetBuffer) {
		int windowId = packetBuffer.readUnsignedByte();
		int slots = packetBuffer.readShort();
		List<ItemStack> itemStacks = NonNullList.withSize(slots, ItemStack.EMPTY);

		for (int j = 0; j < slots; ++j) {
			itemStacks.set(j, PacketHelper.readItemStack(packetBuffer));
		}

		return new SyncContainerStacksMessage(windowId, itemStacks);
	}

	static void onMessage(SyncContainerStacksMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncContainerStacksMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || !(player.containerMenu instanceof BackpackContainer) || player.containerMenu.containerId != msg.windowId) {
			return;
		}
		player.containerMenu.setAll(msg.itemStacks);
	}
}
