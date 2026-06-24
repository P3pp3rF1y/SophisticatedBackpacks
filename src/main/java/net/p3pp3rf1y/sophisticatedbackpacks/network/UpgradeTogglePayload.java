package net.p3pp3rf1y.sophisticatedbackpacks.network;

import io.netty.buffer.ByteBuf;
import net.minecraft.network.chat.Component;
import net.minecraft.network.codec.ByteBufCodecs;
import net.minecraft.network.codec.StreamCodec;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
import net.minecraft.world.entity.player.Player;
import net.neoforged.neoforge.network.handling.IPayloadContext;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.PlayerInventoryProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;

import java.util.Map;

public record UpgradeTogglePayload(int upgradeSlot) implements CustomPacketPayload {
	public static final Type<UpgradeTogglePayload> TYPE = new Type<>(SophisticatedBackpacks.getRL("upgrade_toggle"));
	public static final StreamCodec<ByteBuf, UpgradeTogglePayload> STREAM_CODEC = StreamCodec.composite(ByteBufCodecs.INT, UpgradeTogglePayload::upgradeSlot,
			UpgradeTogglePayload::new);

	@Override
	public Type<? extends CustomPacketPayload> type() {
		return TYPE;
	}

	public static void handlePayload(UpgradeTogglePayload payload, IPayloadContext context) {
		Player player = context.player();
		PlayerInventoryProvider.get().runOnBackpacks(player, (backpack, inventoryName, identifier, slot) -> {
			Map<Integer, IUpgradeWrapper> slotWrappers = BackpackWrapper.fromStack(backpack).getUpgradeHandler().getSlotWrappers();
			if (slotWrappers.containsKey(payload.upgradeSlot)) {
				IUpgradeWrapper upgradeWrapper = slotWrappers.get(payload.upgradeSlot);
				if (upgradeWrapper.canBeDisabled()) {
					upgradeWrapper.setEnabled(!upgradeWrapper.isEnabled());
					String translKey = upgradeWrapper.isEnabled()
							? "gui.sophisticatedbackpacks.status.upgrade_switched_on"
							: "gui.sophisticatedbackpacks.status.upgrade_switched_off";
					player.displayClientMessage(Component.translatable(translKey, upgradeWrapper.getUpgradeStack().getHoverName()), true);
				}
			}
			return true;
		});
	}
}
