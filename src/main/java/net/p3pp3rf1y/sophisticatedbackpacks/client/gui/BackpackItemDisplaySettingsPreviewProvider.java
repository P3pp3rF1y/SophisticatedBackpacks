package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.LinkedStorageBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.IItemDisplaySettingsPreviewProvider;
import net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay.ItemDisplaySettingsContainer;

import java.util.Optional;

public final class BackpackItemDisplaySettingsPreviewProvider implements IItemDisplaySettingsPreviewProvider {
	public static final BackpackItemDisplaySettingsPreviewProvider INSTANCE = new BackpackItemDisplaySettingsPreviewProvider();

	private BackpackItemDisplaySettingsPreviewProvider() {
	}

	@Override
	public Optional<ItemStack> getItemDisplaySettingsPreviewStack(SettingsScreen screen, ItemDisplaySettingsContainer container, int selectedSlot) {
		if (!(screen.getMenu().getStorageWrapper() instanceof IBackpackWrapper backpackWrapper)) {
			return Optional.empty();
		}
		if (backpackWrapper instanceof LinkedStorageBackpackWrapper linkedBackpack) {
			// Settings controls update the client canonical host before the server projection returns.
			linkedBackpack.refreshPhysicalProjection();
		}
		return Optional.of(backpackWrapper.getBackpack().copy());
	}

	@Override
	public float getItemDisplayPreviewYAxisRotation(float yAxisRotation) {
		return yAxisRotation + 180;
	}
}
