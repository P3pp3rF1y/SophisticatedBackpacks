package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.FriendlyByteBuf;
import net.minecraft.world.entity.player.Player;
import net.minecraftforge.fmllegacy.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;

import javax.annotation.Nullable;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class SyncPlayerSettingsMessage {
	@Nullable
	private final CompoundTag settingsNbt;

	public SyncPlayerSettingsMessage(@Nullable CompoundTag settingsNbt) {
		this.settingsNbt = settingsNbt;
	}

	public static void encode(SyncPlayerSettingsMessage msg, FriendlyByteBuf packetBuffer) {
		packetBuffer.writeNbt(msg.settingsNbt);
	}

	public static SyncPlayerSettingsMessage decode(FriendlyByteBuf packetBuffer) {
		return new SyncPlayerSettingsMessage(packetBuffer.readNbt());
	}

	static void onMessage(SyncPlayerSettingsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncPlayerSettingsMessage msg) {
		LocalPlayer player = Minecraft.getInstance().player;
		if (player == null || msg.settingsNbt == null) {
			return;
		}
		//need to call the static call indirectly otherwise this message class is class loaded during packethandler init and crashes on server due to missing ClientPlayerEntity
		BiConsumer<Player, CompoundTag> setSettings = BackpackSettingsManager::setPlayerBackpackSettingsTag;
		setSettings.accept(player, msg.settingsNbt);
	}
}
