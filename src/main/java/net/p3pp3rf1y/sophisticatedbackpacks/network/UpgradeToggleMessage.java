package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.function.Supplier;

public class UpgradeToggleMessage {
	private final int upgradeSlot;

	public UpgradeToggleMessage(int upgradeSlot) {
		this.upgradeSlot = upgradeSlot;
	}

	public static void encode(UpgradeToggleMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.upgradeSlot);
	}

	public static UpgradeToggleMessage decode(FriendlyByteBuf packetBuffer) {
		return new UpgradeToggleMessage(packetBuffer.readInt());
	}

	static void onMessage(UpgradeToggleMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, UpgradeToggleMessage msg) {
		if (player == null) {
			return;
		}

		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(w -> {
				Map<Integer, IUpgradeWrapper> slotWrappers = w.getUpgradeHandler().getSlotWrappers();
				if (slotWrappers.containsKey(msg.upgradeSlot)) {
					IUpgradeWrapper upgradeWrapper = slotWrappers.get(msg.upgradeSlot);
					if (upgradeWrapper.canBeDisabled()) {
						upgradeWrapper.setEnabled(!upgradeWrapper.isEnabled());
						String translKey = upgradeWrapper.isEnabled() ? "gui.sophisticatedbackpacks.status.upgrade_switched_on" : "gui.sophisticatedbackpacks.status.upgrade_switched_off";
						player.displayClientMessage(new TranslatableComponent(translKey, upgradeWrapper.getUpgradeStack().getHoverName()), true);
					}
				}
			});
			return true;
		});
	}
}