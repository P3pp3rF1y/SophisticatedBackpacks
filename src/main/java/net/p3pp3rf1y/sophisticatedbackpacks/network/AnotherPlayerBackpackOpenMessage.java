package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.SimpleMenuProvider;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.network.NetworkEvent;
import net.minecraftforge.network.NetworkHooks;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContext;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.BackpackMainSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;
import net.p3pp3rf1y.sophisticatedcore.settings.main.MainSettingsCategory;

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
		if (player == null || Boolean.FALSE.equals(Config.SERVER.allowOpeningOtherPlayerBackpacks.get())) {
			return;
		}

		if (player.level.getEntity(msg.anotherPlayerId) instanceof Player anotherPlayer) {
			PlayerInventoryProvider.get().runOnBackpacks(anotherPlayer, (backpack, inventoryName, identifier, slot) -> {
				if (canAnotherPlayerOpenBackpack(anotherPlayer, backpack)) {

					BackpackContext.AnotherPlayer backpackContext = new BackpackContext.AnotherPlayer(inventoryName, identifier, slot, anotherPlayer);
					NetworkHooks.openGui(player, new SimpleMenuProvider((w, p, pl) -> new BackpackContainer(w, pl, backpackContext), backpack.getHoverName()),
							backpackContext::toBuffer);
				} else {
					player.displayClientMessage(new TranslatableComponent("gui.sophisticatedbackpacks.status.backpack_cannot_be_open_by_another_player"), true);
				}
				return true;
			}, true);
		}
	}

	private static boolean canAnotherPlayerOpenBackpack(Player anotherPlayer, ItemStack backpack) {
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(wrapper -> {
			MainSettingsCategory category = wrapper.getSettingsHandler().getGlobalSettingsCategory();
			return SettingsManager.getSettingValue(anotherPlayer, category.getPlayerSettingsTagName(), category, BackpackMainSettingsCategory.ANOTHER_PLAYER_CAN_OPEN);
		}).orElse(false);
	}
}
