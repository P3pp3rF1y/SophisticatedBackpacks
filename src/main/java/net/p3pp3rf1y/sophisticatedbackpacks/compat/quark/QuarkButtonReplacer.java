package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraft.client.gui.widget.button.Button;
import net.minecraft.inventory.container.Slot;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import vazkii.quark.content.management.client.gui.MiniInventoryButton;

class QuarkButtonReplacer implements BackpackScreen.IButtonReplacer {
	private static final int INSERT_TYPE = 1;
	private static final int EXTRACT_TYPE = 2;
	private static final int SORT_TYPE = 0;

	@Override
	public boolean shouldReplace(BackpackScreen screen, Button button) {
		if (!(button instanceof MiniInventoryButton)) {
			return false;
		}
		try {
			//this logic isn't ideal by far, but best that can be used to identify the actual sort backpack button
			MiniInventoryButton miniInventoryButton = ((MiniInventoryButton) button);
			int type = ObfuscationReflectionHelper.getPrivateValue(MiniInventoryButton.class, miniInventoryButton, "type");
			Slot slot = screen.getMenu().getSlot(8);
			int x = screen.getGuiLeft() + slot.x + 6;
			int y = screen.getGuiTop() + slot.y - 13;
			return isBackpackSortButton(button, type, x, y) || isInsertButton(type) || isExtractButton(type);
		}
		catch (ObfuscationReflectionHelper.UnableToFindFieldException | ObfuscationReflectionHelper.UnableToAccessFieldException | NullPointerException ex) {
			SophisticatedBackpacks.LOGGER.error("Error checking for Quark Sort button ", ex);
			return false;
		}
	}

	private boolean isExtractButton(int type) {
		return type == EXTRACT_TYPE;
	}

	private boolean isInsertButton(int type) {
		return type == INSERT_TYPE;
	}

	private boolean isBackpackSortButton(Button button, int type, int x, int y) {
		return type == SORT_TYPE && button.x == x && button.y == y;
	}

	@Override
	public Button replace(BackpackScreen screen, Button button) {
		MiniInventoryButton miniInventoryButton = ((MiniInventoryButton) button);
		int type = ObfuscationReflectionHelper.getPrivateValue(MiniInventoryButton.class, miniInventoryButton, "type");
		Slot slot = screen.getMenu().getSlot(8);
		int x = screen.getGuiLeft() + slot.x + 6;
		int y = screen.getGuiTop() + slot.y - 13;
		if (isBackpackSortButton(button, type, x, y)) {
			return new MiniInventoryButton(screen, SORT_TYPE, button.x - screen.getGuiLeft(), button.y - screen.getGuiTop(), "quark.gui.button.sort_container",
					b -> screen.getMenu().sort());
		} else if (isInsertButton(type)) {
			return new MiniInventoryButton(screen, INSERT_TYPE, button.x - screen.getGuiLeft(), button.y - screen.getGuiTop(), "quark.gui.button.insert",
					b -> PacketHandler.sendToServer(new TransferMessage(false)));
		} else if (isExtractButton(type)) {
			return new MiniInventoryButton(screen, EXTRACT_TYPE, button.x - screen.getGuiLeft(), button.y - screen.getGuiTop(), "quark.gui.button.extract",
					b -> PacketHandler.sendToServer(new TransferMessage(true)));
		}
		return button;
	}
}
