package net.p3pp3rf1y.sophisticatedbackpacks.network;

import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.math.BlockPos;
import net.minecraft.util.text.TranslationTextComponent;
import net.minecraftforge.fml.network.NetworkEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBlockToolSwapUpgrade;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

public class BlockToolSwapMessage {
	private final BlockPos pos;

	public BlockToolSwapMessage(BlockPos pos) {
		this.pos = pos;
	}

	public static void encode(BlockToolSwapMessage msg, PacketBuffer packetBuffer) {
		packetBuffer.writeLong(msg.pos.asLong());
	}

	public static BlockToolSwapMessage decode(PacketBuffer packetBuffer) {
		return new BlockToolSwapMessage(BlockPos.of(packetBuffer.readLong()));
	}

	static void onMessage(BlockToolSwapMessage msg, Supplier<NetworkEvent.Context> contextSupplier) {
		NetworkEvent.Context context = contextSupplier.get();
		context.enqueueWork(() -> handleMessage(msg, context.getSender()));
		context.setPacketHandled(true);
	}

	private static void handleMessage(BlockToolSwapMessage msg, @Nullable ServerPlayerEntity sender) {
		if (sender == null) {
			return;
		}
		AtomicBoolean result = new AtomicBoolean(false);
		AtomicBoolean anyUpgradeCanInteract = new AtomicBoolean(false);
		SophisticatedBackpacks.PROXY.getPlayerInventoryProvider().runOnBackpacks(sender, (backpack, inventoryName, slot) -> backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(backpackWrapper -> {
					backpackWrapper.getUpgradeHandler().getWrappersThatImplement(IBlockToolSwapUpgrade.class)
							.forEach(upgrade -> {
								if (!upgrade.canProcessBlockInteract() || result.get()) {
									return;
								}
								anyUpgradeCanInteract.set(true);

								result.set(upgrade.onBlockInteract(sender.level, msg.pos, sender.level.getBlockState(msg.pos), sender));
							});
							return result.get();
						}
				).orElse(false)
		);

		if (!anyUpgradeCanInteract.get()) {
			sender.displayClientMessage(new TranslationTextComponent("gui.sophisticatedbackpacks.status.no_tool_swap_upgrade_present"), true);
			return;
		}
		if (!result.get()) {
			sender.displayClientMessage(new TranslationTextComponent("gui.sophisticatedbackpacks.status.no_tool_found_for_block"), true);
		}
	}
}
