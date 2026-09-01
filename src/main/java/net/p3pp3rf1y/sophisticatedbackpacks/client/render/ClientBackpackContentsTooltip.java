package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestLinkedStorageBackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;

import java.util.UUID;

public class ClientBackpackContentsTooltip extends ClientStorageContentsTooltipBase {
	private static final int ROLE_HEADER_HEIGHT = 18;
	private final ItemStack backpack;

	@SuppressWarnings("unused")
	// parameter needs to be there so that addListener logic would know which event this method listens to
	public static void onWorldLoad(LevelEvent.Load event) {
		refreshContents();
		lastRequestTime = 0;
	}

	@Override
	public void renderImage(Font font, int x, int y, int width, int height, GuiGraphics guiGraphics) {
		BackpackItem.getLinkedStorageEndpointRole(backpack).ifPresent(role -> ClientLinkedStorageTooltip.renderRole(font, x, y, guiGraphics, role,
				backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT).groupId()));
		renderTooltip(getBackpackWrapper(), font, x, y + getRoleHeaderHeight(), guiGraphics);
	}

	@Override
	public int getWidth(Font font) {
		return Math.max(super.getWidth(font), BackpackItem.getLinkedStorageEndpointRole(backpack).map(
				role -> 16 + font.width(ClientLinkedStorageTooltip.getDescription(role, backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT).groupId())))
				.orElse(0));
	}

	@Override
	public int getHeight(Font font) {
		return super.getHeight(font) + getRoleHeaderHeight();
	}

	public ClientBackpackContentsTooltip(BackpackItem.BackpackContentsTooltip tooltip) {
		backpack = tooltip.getBackpack();
	}

	private IBackpackWrapper getBackpackWrapper() {
		if (BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent()) {
			LinkedStorageEndpointData endpoint = backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
			if (ClientLinkedStorageBackpackContents.requestGroupName(endpoint.groupId())) {
				PacketDistributor.sendToServer(new RequestLinkedStorageBackpackContentsPayload(endpoint.groupId(), -1L));
			}
			return BackpackLinkedStorageResolver.resolve(Minecraft.getInstance().level, backpack).orElse(IBackpackWrapper.Noop.INSTANCE);
		}
		return BackpackWrapper.fromStack(backpack);
	}

	private int getRoleHeaderHeight() {
		return BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent() ? ROLE_HEADER_HEIGHT : 0;
	}

	@Override
	protected void sendInventorySyncRequest(UUID uuid) {
		if (BackpackItem.getLinkedStorageEndpointRole(backpack).isPresent()) {
			PacketDistributor
					.sendToServer(new RequestLinkedStorageBackpackContentsPayload(uuid, ClientLinkedStorageBackpackContents.getRevision(uuid).orElse(-1L)));
		} else {
			PacketDistributor.sendToServer(new RequestBackpackInventoryContentsPayload(uuid));
		}
	}
}
