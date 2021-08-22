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
	public void refreshContainer(Container containerToSend, NonNullList<ItemStack> itemsList) {
		PacketHandler.sendToClient(player, new SyncContainerStacksMessage(containerToSend.containerId, itemsList));
		player.connection.send(new SSetSlotPacket(-1, -1, player.inventory.getCarried()));
	}

	@Override
	public void slotChanged(Container containerToSend, int slotInd, ItemStack stack) {
		if (!(containerToSend.getSlot(slotInd) instanceof CraftingResultSlot) && !player.ignoreSlotUpdateHack) {
			PacketHandler.sendToClient(player, new SyncSlotStackMessage(containerToSend.containerId, slotInd, stack));
		}
	}

	@Override
	public void setContainerData(Container containerIn, int varToUpdate, int newValue) {
		//noop - not used in BackpackContainer
	}
}
