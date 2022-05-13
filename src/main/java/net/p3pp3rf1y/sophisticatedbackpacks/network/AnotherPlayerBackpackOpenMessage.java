package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.InventoryMenu;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IContextAwareContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class AnotherPlayerBackpackOpenMessage {
	private final int anotherPlayerId;

	public AnotherPlayerBackpackOpenMessage(int anotherPlayerId) {
		this.anotherPlayerId = anotherPlayerId;
	}

	public static void encode(AnotherPlayerBackpackOpenMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeInt(msg.anotherPlayerId);
	}

	public static AnotherPlayerBackpackOpenMessage decode(FriendlyByteBuf packetBuffer) {
		return new AnotherPlayerBackpackOpenMessage(packetBuffer.readInt());
	}

	static void onMessage(AnotherPlayerBackpackOpenMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(@Nullable ServerPlayer player, AnotherPlayerBackpackOpenMessage msg) {
		if (player == null) {
			return;
		}

		if (player.level.getEntity(msg.anotherPlayerId) instanceof Player anotherPlayer) {
			PlayerInventoryProvider.get().runOnBackpacks(anotherPlayer, (backpack, inventoryName, slot) -> {
				BackpackContext.AnotherPlayer backpackContext = new BackpackContext.AnotherPlayer(inventoryName, slot, anotherPlayer);
				NetworkHooks.openGui(player, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName()),
						backpackContext::toBuffer);
				return true;
			}, true);
		}
	}
}
