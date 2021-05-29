package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import net.minecraft.inventory.container.Slot;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.ITextProperties;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsTabBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SlotSettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonBase;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;

import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.IntConsumer;

public abstract class SettingsTab<C extends SettingsContainerBase<?>> extends SettingsTabBase<SlotSettingsScreen> {
	private final C settingsContainer;

	protected SettingsTab(C settingsContainer, Position position, SlotSettingsScreen screen, ITextComponent tabLabel, List<ITextProperties> tooltip,
			List<ITextProperties> openTooltip, Function<IntConsumer, ButtonBase> getTabButton) {
		super(position, screen, tabLabel, tooltip, openTooltip, getTabButton);
		this.settingsContainer = settingsContainer;
	}

	protected C getSettingsContainer() {
		return settingsContainer;
	}

	public abstract Optional<Integer> getSlotOverlayColor(int slotNumber);

	public abstract void handleSlotClick(Slot slot, int mouseButton);
}
