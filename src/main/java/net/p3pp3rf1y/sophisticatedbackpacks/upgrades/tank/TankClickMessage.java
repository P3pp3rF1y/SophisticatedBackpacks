package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.network.PacketBuffer;
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class TankClickMessage {
	private final int upgradeSlot;

	public TankClickMessage(int upgradeSlot) {
		this.upgradeSlot = upgradeSlot;
	}

	public static void encode(TankClickMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.upgradeSlot);
	}

	public static TankClickMessage decode(PacketBuffer packetBuffer) {
		return new TankClickMessage(packetBuffer.readInt());
	}

	public static void onMessage(TankClickMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity sender, TankClickMessage msg) {
		if (sender == null || !(sender.containerMenu instanceof BackpackContainer)) {
			return;
		}
		UpgradeContainerBase<?, ?> upgradeContainer = ((BackpackContainer) sender.containerMenu).getUpgradeContainers().get(msg.upgradeSlot);
		if (!(upgradeContainer instanceof TankUpgradeContainer)) {
			return;
		}
		TankUpgradeContainer tankContainer = (TankUpgradeContainer) upgradeContainer;
		ItemStack cursorStack = sender.inventory.getCarried();
		cursorStack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).ifPresent(fluidHandler -> {
			TankUpgradeWrapper tankWrapper = tankContainer.getUpgradeWrapper();
			FluidStack tankContents = tankWrapper.getContents();
			if (tankContents.isEmpty()) {
				drainHandler(sender, fluidHandler, tankWrapper);
			} else {
				if (!tankWrapper.fillHandler(fluidHandler, itemStackIn -> {
					sender.inventory.setCarried(itemStackIn);
					sender.connection.send(new SSetSlotPacket(-1, -1, sender.inventory.getCarried()));
				})) {
					drainHandler(sender, fluidHandler, tankWrapper);
				}
			}
		});
	}

	private static void drainHandler(ServerPlayerEntity sender, net.minecraftforge.fluids.capability.IFluidHandlerItem fluidHandler, TankUpgradeWrapper tankWrapper) {
		tankWrapper.drainHandler(fluidHandler, itemStackIn -> {
			sender.inventory.setCarried(itemStackIn);
			sender.connection.send(new SSetSlotPacket(-1, -1, sender.inventory.getCarried()));
		});
	}
}
