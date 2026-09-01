package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsPayload;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestLinkedStorageBackpackContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;

import java.util.Optional;
import java.util.UUID;

public class ClientBackpackContentsTooltip extends ClientStorageContentsTooltipBase {
	private static final int LINKED_HEADER_HEIGHT = 16;

	private final ItemStack backpack;
	private final Optional<LinkedStorageEndpointRole> linkedStorageRole;
	private final Optional<UUID> linkedStorageGroupId;

	@SuppressWarnings("unused")
	// parameter needs to be there so that addListener logic would know which event this method listens to
	public static void onWorldLoad(LevelEvent.Load event) {
		refreshContents();
		lastRequestTime = 0;
	}

	@Override
	public void renderImage(Font font, int x, int y, int width, int height, GuiGraphics guiGraphics) {
		if (linkedStorageGroupId.isPresent()) {
			UUID groupId = linkedStorageGroupId.get();
			if (ClientLinkedStorageBackpackContents.getGroupName(groupId).isEmpty() && ClientLinkedStorageBackpackContents.requestGroupName(groupId)) {
				ClientPacketDistributor.sendToServer(new RequestLinkedStorageBackpackContentsPayload(groupId, -1L));
			}
			ClientLinkedStorageTooltip.renderRole(font, x, y, guiGraphics, linkedStorageRole.orElseThrow(), groupId);
			if (Minecraft.getInstance().level != null) {
				BackpackLinkedStorageResolver.resolve(Minecraft.getInstance().level, backpack)
						.ifPresent(wrapper -> renderTooltip(wrapper, font, x, y + LINKED_HEADER_HEIGHT, guiGraphics));
			}
			return;
		}
		renderTooltip(BackpackWrapper.fromStack(backpack), font, x, y, guiGraphics);
	}

	public ClientBackpackContentsTooltip(BackpackItem.BackpackContentsTooltip tooltip) {
		backpack = tooltip.getBackpack();
		linkedStorageRole = BackpackItem.getLinkedStorageEndpointRole(backpack);
		linkedStorageGroupId = linkedStorageRole.map(role -> backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT).groupId());
	}

	@Override
	public int getWidth(Font font) {
		return linkedStorageRole.map(
				role -> Math.max(super.getWidth(font), 16 + font.width(ClientLinkedStorageTooltip.getDescription(role, linkedStorageGroupId.orElseThrow()))))
				.orElseGet(() -> super.getWidth(font));
	}

	@Override
	public int getHeight(Font font) {
		return super.getHeight(font) + (linkedStorageRole.isPresent() ? LINKED_HEADER_HEIGHT : 0);
	}

	@Override
	protected void sendInventorySyncRequest(UUID uuid) {
		if (linkedStorageGroupId.isPresent()) {
			ClientPacketDistributor
					.sendToServer(new RequestLinkedStorageBackpackContentsPayload(uuid, ClientLinkedStorageBackpackContents.getRevision(uuid).orElse(-1L)));
		} else {
			ClientPacketDistributor.sendToServer(new RequestBackpackInventoryContentsPayload(uuid));
		}
	}
}
