package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockPickResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;

public record BlockPickPayload(ItemStack filter) implements CustomPacketPayload {
	public static final Type<BlockPickPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("block_pick"));
	public static final StreamCodec<RegistryFriendlyByteBuf, BlockPickPayload> STREAM_CODEC = StreamCodec.composite(ItemStack.STREAM_CODEC,
			BlockPickPayload::filter, BlockPickPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(BlockPickPayload payload, IPayloadContext context) {
		Player player = context.player();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryHandlerName, identifier, slot) -> {
			IBackpackWrapper wrapper = BackpackLinkedStorageResolver.resolveForGlobalUpgradeProcessing(player.level(), backpack);
			for (IBlockPickResponseUpgrade upgrade : wrapper.getUpgradeHandler().getWrappersThatImplement(IBlockPickResponseUpgrade.class)) {
				if (upgrade.pickBlock(player, payload.filter)) {
					return true;
				}
			}
			return false;
		});
	}
}
