package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphicsExtractor;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.neoforged.neoforge.event.level.LevelEvent;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackLinkedStorageResolver;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.RequestBackpackInventoryContentsPayload;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.render.ClientStorageContentsTooltipBase;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.ClientLinkedStorageContents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.RequestLinkedStorageContentsPayload;

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
	public void extractImage(Font font, int x, int y, int width, int height, GuiGraphicsExtractor guiGraphics) {
		if (linkedStorageGroupId.isPresent()) {
			UUID groupId = linkedStorageGroupId.get();
			ClientLinkedStorageTooltip.renderRole(font, x, y, guiGraphics, linkedStorageRole.orElseThrow(), groupId);
			extractTooltip(font, x, y + LINKED_HEADER_HEIGHT, guiGraphics);
			return;
		}
		extractTooltip(font, x, y, guiGraphics);
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
	protected IStorageWrapper getTooltipStorageWrapper() {
		if (linkedStorageGroupId.isPresent()) {
			UUID groupId = linkedStorageGroupId.get();
			Minecraft minecraft = Minecraft.getInstance();
			if (minecraft.level != null && ClientLinkedStorageContents.shouldRequestSnapshot(groupId, minecraft.level.getGameTime())) {
				ClientPacketDistributor
						.sendToServer(new RequestLinkedStorageContentsPayload(groupId, ClientLinkedStorageContents.getRevision(groupId).orElse(-1L)));
			}
			return minecraft.level == null
					? IBackpackWrapper.Noop.INSTANCE
					: BackpackLinkedStorageResolver.resolve(minecraft.level, backpack).orElse(IBackpackWrapper.Noop.INSTANCE);
		}
		return BackpackWrapper.fromStack(backpack);
	}

	@Override
	protected void sendInventorySyncRequest(UUID uuid) {
		if (linkedStorageGroupId.isPresent()) {
			ClientPacketDistributor.sendToServer(new RequestLinkedStorageContentsPayload(uuid, ClientLinkedStorageContents.getRevision(uuid).orElse(-1L)));
		} else {
			ClientPacketDistributor.sendToServer(new RequestBackpackInventoryContentsPayload(uuid));
		}
	}
}
