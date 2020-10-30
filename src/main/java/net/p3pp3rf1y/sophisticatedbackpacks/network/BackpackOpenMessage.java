package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.SimpleNamedContainerProvider;
import net.minecraftforge.fml.network.NetworkEvent;
import net.minecraftforge.fml.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import java.util.function.Supplier;

public class BackpackOpenMessage {
	private BackpackOpenMessage() {}

	public static BackpackOpenMessage getInstance() {
		//noinspection InstantiationOfUtilityClass
		return new BackpackOpenMessage();
	}

	static void onMessage(Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(ServerPlayerEntity player) {
		PlayerInventoryProvider.runOnBackpacks(player, (backpack, inventoryName, slot) -> {
			NetworkHooks.openGui(player, new SimpleNamedContainerProvider((w, p, pl) -> new BackpackContainer(w, pl, inventoryName, slot), backpack.getDisplayName()),
					buf -> {
						buf.writeString(inventoryName);
						buf.writeInt(slot);
					});
			return true;
		});
	}
}
