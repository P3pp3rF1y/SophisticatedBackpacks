package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import com.google.common.collect.ImmutableMap;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;

public class InceptionUpgradeTab extends UpgradeSettingsTab<InceptionUpgradeContainer> {
	public static final UpgradeContainerType<InceptionUpgradeItem.Wrapper, InceptionUpgradeContainer> TYPE = new UpgradeContainerType<>(InceptionUpgradeContainer::new);

	private static final ButtonDefinition.Toggle<InventoryOrder> INVENTORY_ORDER = createToggleButtonDefinition(
			ImmutableMap.of(
					InventoryOrder.MAIN_FIRST, GuiHelper.getButtonStateData(new UV(80, 64), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("inventory_order_main_first"), null)),
					InventoryOrder.INCEPTED_FIRST, GuiHelper.getButtonStateData(new UV(96, 64), Dimension.SQUARE_16, new Position(1, 1),
							TranslationHelper.getTranslatedLines(translUpgradeButton("inventory_order_incepted_first"), null))
			));

	public InceptionUpgradeTab(InceptionUpgradeContainer upgradeContainer, Position position, BackpackScreen screen) {
		super(upgradeContainer, position, screen, new TranslationTextComponent(translUpgrade("inception")),
				new TranslationTextComponent(translUpgradeTooltip("inception")));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), INVENTORY_ORDER, button -> getContainer().setInventoryOrder(getContainer().getInventoryOrder().next()),
				() -> getContainer().getInventoryOrder()));
	}

	@Override
	protected void moveSlotsToTab() {
		//noop
	}
}
