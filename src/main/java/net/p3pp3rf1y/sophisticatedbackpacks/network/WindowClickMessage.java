package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SConfirmTransactionPacket;
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraft.util.NonNullList;
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
		packetBuffer.writeEnumValue(msg.clickType);
		PacketHelper.writeItemStack(msg.clickedItem, packetBuffer);
		packetBuffer.writeShort(msg.actionNumber);
	}

	public static WindowClickMessage decode(PacketBuffer packetBuffer) {
		return new WindowClickMessage(packetBuffer.readByte(), packetBuffer.readShort(), packetBuffer.readByte(), packetBuffer.readEnumValue(ClickType.class),
				PacketHelper.readItemStack(packetBuffer), packetBuffer.readShort());
	}

	static void onMessage(WindowClickMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, WindowClickMessage msg) {
		if (player == null || player.openContainer.windowId != msg.windowId || !(player.openContainer instanceof BackpackContainer)) {
			return;
		}

		player.markPlayerActive();
		if (player.isSpectator()) {
			syncSlotsForSpectator(player);
		} else {
			ItemStack stackClickResult = player.openContainer.slotClick(msg.slotNumber, msg.mouseButton, msg.clickType, player);
			if (ItemStack.areItemStacksEqual(msg.clickedItem, stackClickResult)) {
				player.connection.sendPacket(new SConfirmTransactionPacket(msg.windowId, msg.actionNumber, true));
				player.isChangingQuantityOnly = true;
				player.openContainer.detectAndSendChanges();
				player.updateHeldItem();
				player.isChangingQuantityOnly = false;
			} else {
				player.connection.sendPacket(new SConfirmTransactionPacket(msg.windowId, msg.actionNumber, false));
				player.openContainer.setCanCraft(player, false);
				NonNullList<ItemStack> stacks = NonNullList.create();

				for (int j = 0; j < player.openContainer.inventorySlots.size(); ++j) {
					ItemStack itemstack = player.openContainer.inventorySlots.get(j).getStack();
					stacks.add(itemstack.isEmpty() ? ItemStack.EMPTY : itemstack);
				}

				PacketHandler.sendToClient(player, new SyncContainerStacksMessage(player.openContainer.windowId, stacks));
				player.connection.sendPacket(new SSetSlotPacket(-1, -1, player.inventory.getItemStack()));
			}
		}
	}

	private static void syncSlotsForSpectator(ServerPlayerEntity player) {
		NonNullList<ItemStack> stacks = NonNullList.create();

		for (int i = 0; i < player.openContainer.inventorySlots.size(); ++i) {
			stacks.add(player.openContainer.inventorySlots.get(i).getStack());
		}

		PacketHandler.sendToClient(player, new SyncContainerStacksMessage(player.openContainer.windowId, stacks));
	}
}
