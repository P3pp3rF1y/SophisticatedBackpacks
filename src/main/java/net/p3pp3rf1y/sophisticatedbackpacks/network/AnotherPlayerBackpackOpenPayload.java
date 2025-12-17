package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategoryData;

public record AnotherPlayerBackpackOpenPayload(int anotherPlayerId) implements CustomPacketPayload {
	public static final Type<AnotherPlayerBackpackOpenPayload> TYPE = new Type<>(SophisticatedBackpacks.getIdentifier("another_player_backpack_open"));
	public static final StreamCodec<ByteBuf, AnotherPlayerBackpackOpenPayload> STREAM_CODEC = StreamCodec.composite(
			ByteBufCodecs.INT,
			AnotherPlayerBackpackOpenPayload::anotherPlayerId,
			AnotherPlayerBackpackOpenPayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(AnotherPlayerBackpackOpenPayload payload, IPayloadContext context) {
		Player player = context.player();
		if (Boolean.FALSE.equals(Config.SERVER.allowOpeningOtherPlayerBackpacks.get())) {
			return;
		}

		if (player.level().getEntity(payload.anotherPlayerId) instanceof Player anotherPlayer) {
			PlayerInventoryProvider.get().runOnBackpacks(anotherPlayer, (backpack, inventoryName, identifier, slot) -> {
				if (canAnotherPlayerOpenBackpack(anotherPlayer, backpack)) {

					BackpackContext.AnotherPlayer backpackContext = new BackpackContext.AnotherPlayer(inventoryName, identifier, slot, anotherPlayer);
					player.openMenu(new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName()), backpackContext::toBuffer);
				} else {
					player.displayClientMessage(Component.translatable("gui.sophisticatedbackpacks.status.backpack_cannot_be_open_by_another_player"), true);
				}
				return true;
			}, true);
		}
	}

	private static boolean canAnotherPlayerOpenBackpack(Player anotherPlayer, ItemStack backpack) {
		return BackpackWrapper.fromStack(backpack).getSettingsHandler().getMainSettingValue(anotherPlayer, MainSettingsCategoryData::anotherPlayerCanOpen);
	}
}
