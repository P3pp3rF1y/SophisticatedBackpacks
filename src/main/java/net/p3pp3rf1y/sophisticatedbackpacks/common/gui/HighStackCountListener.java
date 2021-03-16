package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.CraftingResultSlot;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.item.ItemStack;
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraft.util.NonNullList;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerStacksMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncSlotStackMessage;

public class HighStackCountListener implements IContainerListener {
	private final ServerPlayerEntity player;

	public HighStackCountListener(ServerPlayerEntity player) {
		this.player = player;
	}

	@Override
	public void sendAllContents(Container containerToSend, NonNullList<ItemStack> itemsList) {
		PacketHandler.sendToClient(player, new SyncContainerStacksMessage(containerToSend.windowId, itemsList));
		player.connection.sendPacket(new SSetSlotPacket(-1, -1, player.inventory.getItemStack()));
	}

	@Override
	public void sendSlotContents(Container containerToSend, int slotInd, ItemStack stack) {
		if (!(containerToSend.getSlot(slotInd) instanceof CraftingResultSlot)) {
			if (!player.isChangingQuantityOnly) {
				PacketHandler.sendToClient(player, new SyncSlotStackMessage(containerToSend.windowId, slotInd, stack));
			}
		}
	}

	@Override
	public void sendWindowProperty(Container containerIn, int varToUpdate, int newValue) {
		//noop - not used in BackpackContainer
	}
}
