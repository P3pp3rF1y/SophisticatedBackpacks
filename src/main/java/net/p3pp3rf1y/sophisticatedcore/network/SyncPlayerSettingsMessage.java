package net.p3pp3rf1y.sophisticatedcore.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedcore.settings.SettingsManager;

import javax.annotation.Nullable;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class SyncPlayerSettingsMessage {
	private final String playerTagName;
	@Nullable
	private final CompoundTag settingsNbt;

	public SyncPlayerSettingsMessage(String playerTagName, @Nullable CompoundTag settingsNbt) {
		this.playerTagName = playerTagName;
		this.settingsNbt = settingsNbt;
	}

	public static void encode(SyncPlayerSettingsMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeUtf(msg.playerTagName);
		packetBuffer.writeNbt(msg.settingsNbt);
	}

	public static SyncPlayerSettingsMessage decode(FriendlyByteBuf packetBuffer) {
		return new SyncPlayerSettingsMessage(packetBuffer.readUtf(), packetBuffer.readNbt());
	}

	public static void onMessage(SyncPlayerSettingsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncPlayerSettingsMessage msg) {
		LocalPlayer localPlayer = Minecraft.getInstance().player;
		if (localPlayer == null || msg.settingsNbt == null) {
			return;
		}
		//need to call the static call indirectly otherwise this message class is class loaded during packethandler init and crashes on server due to missing ClientPlayerEntity
		BiConsumer<Player, CompoundTag> setSettings = (player, settingsNbt) -> SettingsManager.setPlayerSettingsTag(player, msg.playerTagName, settingsNbt);
		setSettings.accept(localPlayer, msg.settingsNbt);
	}
}
