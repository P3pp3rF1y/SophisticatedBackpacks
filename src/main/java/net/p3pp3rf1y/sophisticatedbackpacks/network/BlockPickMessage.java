package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockPickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

import javax.annotation.Nullable;
import java.util.function.Supplier;

public class BlockPickMessage {
	private final ItemStack filter;

	public BlockPickMessage(ItemStack filter) {
		this.filter = filter;
	}
	static void onMessage(BlockPickMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(context.getSender(), msg));
		context.setPacketHandled(true);
	}

	public static void encode(BlockPickMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeItemStack(msg.filter, false);
	}

	public static BlockPickMessage decode(FriendlyByteBuf packetBuffer) {
		return new BlockPickMessage(packetBuffer.readItem());
	}
	private static void handleMessage(@Nullable ServerPlayer player, BlockPickMessage msg) {
		if (player == null) {
			return;
		}

		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(wrapper -> {
					for (IBlockPickResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IBlockPickResponseUpgrade.class)) {
						if (upgrade.pickBlock(player, msg.filter)) {
							return true;
						}
					}
					return false;
				}).orElse(false));
	}
}
