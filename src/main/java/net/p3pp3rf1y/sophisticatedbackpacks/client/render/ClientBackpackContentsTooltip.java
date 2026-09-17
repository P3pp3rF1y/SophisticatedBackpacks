package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.event.level.LevelEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SBPPacketHandler;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;
import net.p3pp3rf1y.sophisticatedcore.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedcore.network.RequestLinkedStorageContentsMessage;

import java.util.UUID;

public class ClientBackpackContentsTooltip extends ClientStorageContentsTooltipBase {
	private static final int ROLE_HEADER_HEIGHT = 18;
	private final ItemStack backpack;

	@SuppressWarnings("unused") // parameter needs to be there so that addListener logic would know which event this method listens to
	public static void onWorldLoad(LevelEvent.Load event) {
		refreshContents();
		lastRequestTime = 0;
	}

	@Override
	public void renderImage(Font font, int leftX, int topY, GuiGraphics guiGraphics) {
		BackpackItem.getLinkedStorageEndpointRole(backpack).ifPresent(role -> {
			LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
			ClientLinkedStorageTooltip.renderRole(font, leftX, topY, guiGraphics, role, endpoint.groupId());
		});
		renderTooltip(getBackpackWrapper(), font, leftX, topY + getRoleHeaderHeight(), guiGraphics);
	}

	@Override
	public int getWidth(Font font) {
		return Math.max(super.getWidth(font), BackpackItem.getLinkedStorageEndpointRole(backpack).map(role -> {
			LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
			return 16 + font.width(ClientLinkedStorageTooltip.getDescription(role, endpoint.groupId()));
		}).orElse(0));
	}

	@Override
	public int getHeight() {
		return super.getHeight() + getRoleHeaderHeight();
	}

	public ClientBackpackContentsTooltip(BackpackItem.BackpackContentsTooltip tooltip) {
		backpack = tooltip.getBackpack();
	}

	private IBackpackWrapper getBackpackWrapper() {
		if (BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent()) {
			LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
			Minecraft minecraft = Minecraft.getInstance();
			if (minecraft.level != null && ClientLinkedStorageContents.shouldRequestSnapshot(endpoint.groupId(), minecraft.level.getGameTime())) {
				PacketHandler.INSTANCE.sendToServer(
						new RequestLinkedStorageContentsMessage(endpoint.groupId(), ClientLinkedStorageContents.getRevision(endpoint.groupId()).orElse(-1L)));
			}
			return BackpackLinkedStorageResolver.resolve(Minecraft.getInstance().level, backpack).orElse(IBackpackWrapper.Noop.INSTANCE);
		}
		return backpack.getCapability(net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper.getCapabilityInstance())
				.orElse(IBackpackWrapper.Noop.INSTANCE);
	}

	private int getRoleHeaderHeight() {
		return BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent() ? ROLE_HEADER_HEIGHT : 0;
	}

	@Override
	protected void sendInventorySyncRequest(UUID uuid) {
		if (BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent()) {
			PacketHandler.INSTANCE.sendToServer(new RequestLinkedStorageContentsMessage(uuid, ClientLinkedStorageContents.getRevision(uuid).orElse(-1L)));
		} else {
			SBPPacketHandler.INSTANCE.sendToServer(new RequestBackpackInventoryContentsMessage(uuid));
		}
	}
}
