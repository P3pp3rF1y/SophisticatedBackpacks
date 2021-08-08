package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SConfirmTransactionPacket;
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class WindowClickMessage {
	private final int windowId;
	private final int slotNumber;
	private final int mouseButton;
	private final ClickType clickType;
	private final ItemStack clickedItem;
	private final short actionNumber;

	public WindowClickMessage(int windowId, int slotNumber, int mouseButton, ClickType clickType, ItemStack clickedItem, short actionNumber) {
		this.windowId = windowId;
		this.slotNumber = slotNumber;
		this.mouseButton = mouseButton;
		this.clickType = clickType;
		this.clickedItem = clickedItem;
		this.actionNumber = actionNumber;
	}

	public static void encode(WindowClickMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeByte(msg.windowId);
		packetBuffer.writeShort(msg.slotNumber);
		packetBuffer.writeByte(msg.mouseButton);
		packetBuffer.writeEnum(msg.clickType);
		PacketHelper.writeItemStack(msg.clickedItem, packetBuffer);
		packetBuffer.writeShort(msg.actionNumber);
	}

	public static WindowClickMessage decode(PacketBuffer packetBuffer) {
		return new WindowClickMessage(packetBuffer.readByte(), packetBuffer.readShort(), packetBuffer.readByte(), packetBuffer.readEnum(ClickType.class),
				PacketHelper.readItemStack(packetBuffer), packetBuffer.readShort());
	}

	static void onMessage(WindowClickMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, WindowClickMessage msg) {
		if (player == null || player.containerMenu.containerId != msg.windowId || !(player.containerMenu instanceof BackpackContainer)) {
			return;
		}

		player.resetLastActionTime();
		if (player.isSpectator()) {
			syncSlotsForSpectator(player);
		} else {
			ItemStack stackClickResult = player.containerMenu.clicked(msg.slotNumber, msg.mouseButton, msg.clickType, player);
			if (ItemStack.matches(msg.clickedItem, stackClickResult)) {
				player.connection.send(new SConfirmTransactionPacket(msg.windowId, msg.actionNumber, true));
				player.ignoreSlotUpdateHack = true;
				player.containerMenu.broadcastChanges();
				player.broadcastCarriedItem();
				player.ignoreSlotUpdateHack = false;
			} else {
				player.connection.send(new SConfirmTransactionPacket(msg.windowId, msg.actionNumber, false));
				player.containerMenu.setSynched(player, false);
				PacketHandler.sendToClient(player, new SyncContainerStacksMessage(player.containerMenu.containerId, player.containerMenu.getItems()));
				player.connection.send(new SSetSlotPacket(-1, -1, player.inventory.getCarried()));
			}
		}
	}

	private static void syncSlotsForSpectator(ServerPlayerEntity player) {
		PacketHandler.sendToClient(player, new SyncContainerStacksMessage(player.containerMenu.containerId, player.containerMenu.getItems()));
	}
}
