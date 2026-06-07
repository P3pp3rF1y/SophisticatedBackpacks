package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import com.mojang.blaze3d.platform.InputConstants;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.client.input.KeyEvent;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.Slot;
import net.neoforged.neoforge.client.network.ClientPacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.KeybindHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.BackpackOpenPayload;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;

public class BackpackScreen extends StorageScreenBase<BackpackContainer> implements IBackpackScreen {
	public static BackpackScreen constructScreen(BackpackContainer screenContainer, Inventory inv, Component title) {
		return new BackpackScreen(screenContainer, inv, title);
	}

	public BackpackScreen(BackpackContainer screenContainer, Inventory inv, Component titleIn) {
		super(screenContainer, inv, titleIn);
	}

	@Override
	public boolean keyPressed(KeyEvent event) {
		if (isTextBoxFocused()) {
			return super.keyPressed(event);
		}

		if (event.key() == 256 || KeybindHandler.BACKPACK_OPEN_KEYBIND.isActiveAndMatches(InputConstants.getKey(event))) {
			if (getFocused() != null && !clearFocusedWidget()) {
				return super.keyPressed(event);
			}
			if (getMenu().isFirstLevelStorage() && (event.key() == 256 || mouseNotOverBackpack())) {
				if (getMenu().getBackpackContext().wasOpenFromInventory()) {
					getMinecraft().player.closeContainer();
					getMinecraft().setScreen(new InventoryScreen(getMinecraft().player));
				} else {
					onClose();
				}
				return true;
			} else if (!getMenu().isFirstLevelStorage()) {
				ClientPacketDistributor.sendToServer(new BackpackOpenPayload());
				return true;
			}
		}
		return super.keyPressed(event);
	}

	private boolean mouseNotOverBackpack() {
		Slot selectedSlot = getSlotUnderMouse();
		return selectedSlot == null || !(selectedSlot.getItem().getItem() instanceof BackpackItem);
	}

	@Override
	protected String getStorageSettingsTabTooltip() {
		return BackpackTranslationHelper.INSTANCE.translGui("settings.tooltip");
	}

	@Override
	public void render(GuiGraphics guiGraphics, int mouseX, int mouseY, float partialTicks) {
		super.render(guiGraphics, mouseX, mouseY, partialTicks);
		if (getMenu().getNumberOfStorageInventorySlots() == 0 && Minecraft.getInstance().player != null) {
			Minecraft.getInstance().player.closeContainer();
		}
	}
}
