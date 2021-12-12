package net.p3pp3rf1y.sophisticatedbackpacks.settings;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.renderer.ItemRenderer;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SettingsTabControl;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.backpack.BackpackSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.itemdisplay.ItemDisplaySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.itemdisplay.ItemDisplaySettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsCategory;
import net.p3pp3rf1y.sophisticatedbackpacks.settings.memory.MemorySettingsTab;
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
		addFactory(builder, MemorySettingsCategory.NAME, MemorySettingsTab::new);
		addFactory(builder, ItemDisplaySettingsCategory.NAME, ItemDisplaySettingsTab::new);
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
		if (colors.isEmpty()) {
			return;
		}

		int stripeHeight = 16 / colors.size();
		int i = 0;
		for (int color : colors) {
			int yOffset = i * stripeHeight;
			overlayRenderer.renderSlotOverlay(matrixStack, slot.x, slot.y + yOffset, i == colors.size() - 1 ? 16 - yOffset : stripeHeight,  color);
			i++;
		}
	}

	public void handleSlotClick(Slot slot, int mouseButton) {
		getOpenTab().ifPresent(tab -> tab.handleSlotClick(slot, mouseButton));
	}

	public void renderGuiItem(ItemRenderer itemRenderer, ItemStack itemstack, Slot slot) {
		for (SettingsTab<?> tab : settingsTabs) {
			int rotation = tab.getItemRotation(slot.index);
			if (rotation != 0) {
				GuiHelper.tryRenderGuiItem(itemRenderer, minecraft.getTextureManager(), minecraft.player, itemstack, slot.x, slot.y, rotation);
				return;
			}
		}
		itemRenderer.renderAndDecorateItem(itemstack, slot.x, slot.y);
	}

	public interface ISlotOverlayRenderer {
		void renderSlotOverlay(MatrixStack matrixStack, int xPos, int yPos, int height, int slotColor);
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
