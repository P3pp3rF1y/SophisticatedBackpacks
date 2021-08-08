package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.inventory.container.Slot;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsTabControl;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.nosort.NoSortSettingsTab;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class BackpackSettingsTabControl extends SettingsTabControl<SettingsScreen, SettingsTab<?>> {
	private static final Map<String, ISettingsTabFactory<?, ?>> SETTINGS_TAB_FACTORIES;
	private final List<SettingsTab<?>> settingsTabs = new ArrayList<>();

	static {
		ImmutableMap.Builder<String, ISettingsTabFactory<?, ?>> builder = new ImmutableMap.Builder<>();
		addFactory(builder, BackpackSettingsCategory.NAME, BackpackSettingsTab::new);
		addFactory(builder, NoSortSettingsCategory.NAME, NoSortSettingsTab::new);
		SETTINGS_TAB_FACTORIES = builder.build();
	}

	public BackpackSettingsTabControl(SettingsScreen screen, Position position) {
		super(position);
		addChild(new BackToBackpackTab(new Position(x, getTopY())));
		screen.getMenu().forEachSettingsContainer((categoryName, settingsContainer) -> settingsTabs.add(addSettingsTab(() -> {}, () -> {},
				instantiateContainer(categoryName, settingsContainer, new Position(x, getTopY()), screen))));
	}

	private static <C extends SettingsContainerBase<?>, T extends SettingsTab<C>> void addFactory(
			ImmutableMap.Builder<String, ISettingsTabFactory<?, ?>> builder, String categoryName, ISettingsTabFactory<C, T> factory) {
		builder.put(categoryName, factory);
	}

	public void renderSlotOverlays(MatrixStack matrixStack, Slot slot, ISlotOverlayRenderer overlayRenderer) {
		List<Integer> colors = new ArrayList<>();
		settingsTabs.forEach(tab -> tab.getSlotOverlayColor(slot.index).ifPresent(colors::add));
		colors.forEach(c -> overlayRenderer.renderSlotOverlay(matrixStack, slot, c));
	}

	public void handleSlotClick(Slot slot, int mouseButton) {
		getOpenTab().ifPresent(tab -> tab.handleSlotClick(slot, mouseButton));
	}

	public interface ISlotOverlayRenderer {
		void renderSlotOverlay(MatrixStack matrixStack, Slot slot, int slotColor);
	}

	public interface ISettingsTabFactory<C extends SettingsContainerBase<?>, T extends SettingsTab<C>> {
		T create(C container, Position position, SettingsScreen screen);
	}

	private static <C extends SettingsContainerBase<?>> SettingsTab<C> instantiateContainer(String categoryName, C container, Position position, SettingsScreen screen) {
		//noinspection unchecked
		return (SettingsTab<C>) getSettingsTabFactory(categoryName).create(container, position, screen);
	}

	private static <C extends SettingsContainerBase<?>, T extends SettingsTab<C>> ISettingsTabFactory<C, T> getSettingsTabFactory(String name) {
		//noinspection unchecked
		return (ISettingsTabFactory<C, T>) SETTINGS_TAB_FACTORIES.get(name);
	}
}
