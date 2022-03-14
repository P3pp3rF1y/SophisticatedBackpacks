package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinition;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;

import java.util.Map;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions.createToggleButtonDefinition;

public class InceptionUpgradeTab extends UpgradeSettingsTab<InceptionUpgradeContainer> {
	public static final UpgradeContainerType<InceptionUpgradeWrapper, InceptionUpgradeContainer> TYPE = new UpgradeContainerType<>(InceptionUpgradeContainer::new);

	private static final ButtonDefinition.Toggle<InventoryOrder> INVENTORY_ORDER = createToggleButtonDefinition(
			Map.of(
					InventoryOrder.MAIN_FIRST, GuiHelper.getButtonStateData(new UV(48, 32), Dimension.SQUARE_16, new Position(1, 1),
							SBPTranslationHelper.INSTANCE.getTranslatedLines(SBPTranslationHelper.INSTANCE.translUpgradeButton("inventory_order_main_first"), null)),
					InventoryOrder.INCEPTED_FIRST, GuiHelper.getButtonStateData(new UV(64, 32), Dimension.SQUARE_16, new Position(1, 1),
							SBPTranslationHelper.INSTANCE.getTranslatedLines(SBPTranslationHelper.INSTANCE.translUpgradeButton("inventory_order_incepted_first"), null))
			));

	public InceptionUpgradeTab(InceptionUpgradeContainer upgradeContainer, Position position, StorageScreenBase<?> screen) {
		super(upgradeContainer, position, screen, SBPTranslationHelper.INSTANCE.translUpgrade("inception"), SBPTranslationHelper.INSTANCE.translUpgradeTooltip("inception"));
		addHideableChild(new ToggleButton<>(new Position(x + 3, y + 24), INVENTORY_ORDER, button -> getContainer().setInventoryOrder(getContainer().getInventoryOrder().next()),
				() -> getContainer().getInventoryOrder()));
	}

	@Override
	protected void moveSlotsToTab() {
		//noop
	}
}
