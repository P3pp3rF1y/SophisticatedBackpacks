package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.protocol.game.ClientboundContainerSetSlotPacket;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class TankClickMessage {
	private final int upgradeSlot;

	public TankClickMessage(int upgradeSlot) {
		this.upgradeSlot = upgradeSlot;
	}

	public static void encode(TankClickMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.upgradeSlot);
	}

	public static TankClickMessage decode(FriendlyByteBuf packetBuffer) {
		return new TankClickMessage(packetBuffer.readInt());
	}

	public static void onMessage(TankClickMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer sender, TankClickMessage msg) {
		if (sender == null || !(sender.containerMenu instanceof BackpackContainer)) {
			return;
		}
		AbstractContainerMenu containerMenu = sender.containerMenu;
		UpgradeContainerBase<?, ?> upgradeContainer = ((BackpackContainer) containerMenu).getUpgradeContainers().get(msg.upgradeSlot);
		if (!(upgradeContainer instanceof TankUpgradeContainer tankContainer)) {
			return;
		}
		ItemStack cursorStack = containerMenu.getCarried();
		cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).ifPresent(fluidHandler -> {
			TankUpgradeWrapper tankWrapper = tankContainer.getUpgradeWrapper();
			FluidStack tankContents = tankWrapper.getContents();
			if (tankContents.isEmpty()) {
				drainHandler(sender, containerMenu, fluidHandler, tankWrapper);
			} else {
				if (!tankWrapper.fillHandler(fluidHandler, itemStackIn -> {
					containerMenu.setCarried(itemStackIn);
					sender.connection.send(new ClientboundContainerSetSlotPacket(-1, containerMenu.incrementStateId(), -1, containerMenu.getCarried()));
				})) {
					drainHandler(sender, containerMenu, fluidHandler, tankWrapper);
				}
			}
		});
	}

	private static void drainHandler(ServerPlayer sender, AbstractContainerMenu containerMenu, IFluidHandlerItem fluidHandler, TankUpgradeWrapper tankWrapper) {
		tankWrapper.drainHandler(fluidHandler, itemStackIn -> {
			containerMenu.setCarried(itemStackIn);
			sender.connection.send(new ClientboundContainerSetSlotPacket(-1, containerMenu.incrementStateId(), -1, containerMenu.getCarried()));
		});
	}
}
