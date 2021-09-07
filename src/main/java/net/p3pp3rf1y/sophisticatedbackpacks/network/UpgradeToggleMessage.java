package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;

import javax.annotation.Nullable;
import java.util.Map;
import java.util.function.Supplier;

public class UpgradeToggleMessage {
	private final int upgradeSlot;

	public UpgradeToggleMessage(int upgradeSlot) {
		this.upgradeSlot = upgradeSlot;
	}

	public static void encode(UpgradeToggleMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeInt(msg.upgradeSlot);
	}

	public static UpgradeToggleMessage decode(PacketBuffer packetBuffer) {
		return new UpgradeToggleMessage(packetBuffer.readInt());
	}

	static void onMessage(UpgradeToggleMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayerEntity player, UpgradeToggleMessage msg) {
		if (player == null) {
			return;
		}

		SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).ifPresent(w -> {
				Map<Integer, IUpgradeWrapper> slotWrappers = w.getUpgradeHandler().getSlotWrappers();
				if (slotWrappers.containsKey(msg.upgradeSlot)) {
					IUpgradeWrapper upgradeWrapper = slotWrappers.get(msg.upgradeSlot);
					upgradeWrapper.setEnabled(!upgradeWrapper.isEnabled());
					String translKey = upgradeWrapper.isEnabled() ? "gui.sophisticatedbackpacks.status.upgrade_switched_on" : "gui.sophisticatedbackpacks.status.upgrade_switched_off";
					player.displayClientMessage(new TranslationTextComponent(translKey, upgradeWrapper.getUpgradeStack().getHoverName()), true);
				}
			});
			return true;
		});
	}
}