package net.p3pp3rf1y.sophisticatedbackpacks.client.gui;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ItemButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;

public abstract class UpgradeSettingsTab<C extends UpgradeContainerBase<?, ?>> extends SettingsTabBase<BackpackScreen> {

	private final C upgradeContainer;

	protected UpgradeSettingsTab(C upgradeContainer, Position position, BackpackScreen screen, Component tabLabel, Component closedTooltip) {
		super(position, screen, tabLabel, closedTooltip,
				onTabIconClicked -> new ItemButton(new Position(position.x() + 1, position.y() + 4), onTabIconClicked, upgradeContainer.getUpgradeStack(), new TranslatableComponent("gui.sophisticatedbackpacks.narrate.tab_button")));
		this.upgradeContainer = upgradeContainer;
		moveSlotsOutOfView();
	}

	protected C getContainer() {
		return upgradeContainer;
	}

	protected abstract void moveSlotsToTab();

	protected void moveSlotsOutOfView() {
		getContainer().getSlots().forEach(slot -> slot.x = BackpackScreen.DISABLED_SLOT_X_POS);
	}

	@Override
	protected void onTabOpen() {
		super.onTabOpen();
		moveSlotsToTab();
	}

	@Override
	protected void onTabClose() {
		super.onTabClose();
		moveSlotsOutOfView();
	}

	@Override
	protected void setOpen(boolean isOpen) {
		upgradeContainer.setIsOpen(isOpen);
		super.setOpen(isOpen);
	}

	public void onAfterInit() {
		if (upgradeContainer.isOpen()) {
			setOpen(true);
		}
	}
}
