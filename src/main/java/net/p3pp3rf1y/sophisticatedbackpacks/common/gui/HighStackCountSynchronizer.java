package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.core.NonNullList;
import net.minecraft.network.protocol.game.ClientboundContainerSetSlotPacket;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ContainerSynchronizer;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerStacksMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncSlotStackMessage;

public class HighStackCountSynchronizer implements ContainerSynchronizer {
	private final ServerPlayer player;

	public HighStackCountSynchronizer(ServerPlayer player) {
		this.player = player;
	}

	@Override
	public void sendInitialData(AbstractContainerMenu containerMenu, NonNullList<ItemStack> stacks, ItemStack carriedStack, int[] dataSlots) {
		PacketHandler.sendToClient(player, new SyncContainerStacksMessage(containerMenu.containerId, containerMenu.incrementStateId(), stacks, carriedStack));
	}

	@Override
	public void sendSlotChange(AbstractContainerMenu containerMenu, int slotInd, ItemStack stack) {
		PacketHandler.sendToClient(player, new SyncSlotStackMessage(containerMenu.containerId, containerMenu.incrementStateId(), slotInd, stack));
	}

	@Override
	public void sendCarriedChange(AbstractContainerMenu containerMenu, ItemStack stack) {
		player.connection.send(new ClientboundContainerSetSlotPacket(-1, containerMenu.incrementStateId(), -1, stack));
	}

	@Override
	public void sendDataChange(AbstractContainerMenu containerMenu, int slotInd, int data) {
		//noop - not used in BackpackContainer
	}
}
