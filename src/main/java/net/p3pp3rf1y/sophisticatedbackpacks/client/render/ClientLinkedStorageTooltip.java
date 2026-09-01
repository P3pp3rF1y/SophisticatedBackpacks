package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.tooltip.ClientTooltipComponent;
import net.minecraft.network.chat.Component;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.ClientLinkedStorageBackpackContents;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.LinkedStorageEndpointRoleRenderer;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointRole;

import javax.annotation.Nullable;

import java.util.UUID;

public class ClientLinkedStorageTooltip implements ClientTooltipComponent {
	public static final int ROLE_TEXT_COLOR = 0xFF_AE8BC7;

	private final LinkedStorageEndpointRole role;
	@Nullable
	private final UUID groupId;

	public ClientLinkedStorageTooltip(BackpackItem.LinkedStorageTooltip tooltip) {
		role = tooltip.role();
		groupId = tooltip.groupId();
	}

	@Override
	public int getHeight(Font font) {
		return 16;
	}

	@Override
	public int getWidth(Font font) {
		return 16 + font.width(getDescription(role, groupId));
	}

	@Override
	public void renderImage(Font font, int x, int y, int width, int height, GuiGraphics guiGraphics) {
		renderRole(font, x, y, guiGraphics, role, groupId);
	}

	public static void renderRole(Font font, int x, int y, GuiGraphics guiGraphics, LinkedStorageEndpointRole role, @Nullable UUID groupId) {
		LinkedStorageEndpointRoleRenderer.renderIcon(guiGraphics, x - 2, y, role);
		guiGraphics.drawString(font, getDescription(role, groupId), x + 16, y + 4, ROLE_TEXT_COLOR, false);
	}

	public static Component getDescription(LinkedStorageEndpointRole role, @Nullable UUID groupId) {
		Component roleDescription = LinkedStorageEndpointRoleRenderer.getDescription(role);
		if (groupId == null) {
			return roleDescription;
		}
		return ClientLinkedStorageBackpackContents.getGroupName(groupId).filter(groupName -> !groupName.getString().isEmpty())
				.<Component>map(groupName -> TranslationHelper.INSTANCE.translTooltip(
						role == LinkedStorageEndpointRole.PRIMARY ? "linked_storage.primary_named" : "linked_storage.secondary_named", roleDescription,
						groupName))
				.orElse(roleDescription);
	}
}
