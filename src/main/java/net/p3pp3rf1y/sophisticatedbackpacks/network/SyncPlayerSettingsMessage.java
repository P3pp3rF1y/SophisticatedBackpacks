package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.client.Minecraft;
import net.minecraft.client.entity.player.ClientPlayerEntity;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackSettingsManager;

import javax.annotation.Nullable;
import java.util.function.BiConsumer;
import java.util.function.Supplier;

public class SyncPlayerSettingsMessage {
	@Nullable
	private final CompoundNBT settingsNbt;

	public SyncPlayerSettingsMessage(@Nullable CompoundNBT settingsNbt) {
		this.settingsNbt = settingsNbt;
	}

	public static void encode(SyncPlayerSettingsMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeNbt(msg.settingsNbt);
	}

	public static SyncPlayerSettingsMessage decode(PacketBuffer packetBuffer) {
		return new SyncPlayerSettingsMessage(packetBuffer.readNbt());
	}

	static void onMessage(SyncPlayerSettingsMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg));
		context.setPacketHandled(true);
	}

	private static void handleMessage(SyncPlayerSettingsMessage msg) {
		ClientPlayerEntity player = Minecraft.getInstance().player;
		if (player == null || msg.settingsNbt == null) {
			return;
		}
		//need to call the static call indirectly otherwise this message class is class loaded during packethandler init and crashes on server due to missing ClientPlayerEntity
		BiConsumer<PlayerEntity, CompoundNBT> setSettings = BackpackSettingsManager::setPlayerBackpackSettingsTag;
		setSettings.accept(player, msg.settingsNbt);
	}
}
