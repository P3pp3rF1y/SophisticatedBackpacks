package net.p3pp3rf1y.sophisticatedcore.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.core.NonNullList;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;

import java.util.List;
import java.util.function.Supplier;

public class SyncContainerStacksMessage {
	private final int windowId;
	private final int stateId;
	private final List<ItemStack> itemStacks;
	private final ItemStack carriedStack;

	public SyncContainerStacksMessage(int windowId, int stateId, List<ItemStack> itemStacks, ItemStack carriedStack) {
		this.windowId = windowId;
		this.stateId = stateId;
		this.itemStacks = itemStacks;
		this.carriedStack = carriedStack;
	}

	public static void encode(SyncContainerStacksMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeByte(msg.windowId);
		packetBuffer.writeVarInt(msg.stateId);
		packetBuffer.writeShort(msg.itemStacks.size());

		for (ItemStack itemstack : msg.itemStacks) {
			PacketHelper.writeItemStack(itemstack, packetBuffer);
		}
		packetBuffer.writeItemStack(msg.carriedStack, true);
	}

	public static SyncContainerStacksMessage decode(FriendlyByteBuf packetBuffer) {
		int windowId = packetBuffer.readUnsignedByte();
		int stateId = packetBuffer.readVarInt();
		int slots = packetBuffer.readShort();
		List<ItemStack> itemStacks = NonNullList.withSize(slots, ItemStack.EMPTY);

		for (int j = 0; j < slots; ++j) {
			itemStacks.set(j, PacketHelper.readItemStack(packetBuffer));
		}

		ItemStack carriedStack = packetBuffer.readItem();

		return new SyncContainerStacksMessage(windowId, stateId, itemStacks, carriedStack);
	}

	public static void onMessage(SyncContainerStacksMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncContainerStacksMessage msg) {
		LocalPlayer player = Minecraft.getInstance().player;
		if (player == null || !(player.containerMenu instanceof StorageContainerMenuBase) || player.containerMenu.containerId != msg.windowId) {
			return;
		}
		player.containerMenu.initializeContents(msg.stateId, msg.itemStacks, msg.carriedStack);
	}
}
