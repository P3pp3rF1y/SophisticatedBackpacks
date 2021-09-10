package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BackpackOpenMessage {
	private final int subBackpackSlotIndex;

	public BackpackOpenMessage() {
		this(-1);
	}

	public BackpackOpenMessage(int subBackpackSlotIndex) {
		this.subBackpackSlotIndex = subBackpackSlotIndex;
	}

	public static void encode(BackpackOpenMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.subBackpackSlotIndex);
	}

	public static BackpackOpenMessage decode(PacketBuffer packetBuffer) {
		return new BackpackOpenMessage(packetBuffer.readInt());
	}

	static void onMessage(BackpackOpenMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, BackpackOpenMessage msg) {
		if (player == null) {
			return;
		}

		if (player.containerMenu instanceof BackpackContainer) {
			BackpackContext backpackContext = ((BackpackContainer) player.containerMenu).getBackpackContext();
			if (msg.subBackpackSlotIndex == -1) {
				openBackpack(player, backpackContext.getParentBackpackContext());
			} else {
				openBackpack(player, backpackContext.getSubBackpackContext(msg.subBackpackSlotIndex));
			}
		} else if (player.containerMenu instanceof IContextAwareContainer) {
			BackpackContext backpackContext = ((IContextAwareContainer) player.containerMenu).getBackpackContext();
			openBackpack(player, backpackContext);
		} else {
			findAndOpenFirstBackpack(player);
		}
	}

	private static void findAndOpenFirstBackpack(ServerPlayerEntity player) {
		SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			BackpackContext.Item backpackContext = new BackpackContext.Item(inventoryName, slot);
			NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName()),
					backpackContext::toBuffer);
			return true;
		});
	}

	private static void openBackpack(ServerPlayerEntity player, BackpackContext backpackContext) {
		NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpackContext.getDisplayName(player)),
				backpackContext::toBuffer);
	}
}
