package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

public class ToolSwapperUpgradeItem extends UpgradeItemBase<ToolSwapperUpgradeWrapper> {
	private static final UpgradeType<ToolSwapperUpgradeWrapper> TYPE = new UpgradeType<>(ToolSwapperUpgradeWrapper::new);
	private final boolean hasSettingsTab;
	private final boolean swapToolOnKeyPress;

	public ToolSwapperUpgradeItem(boolean hasSettingsTab, boolean swapToolOnKeyPress) {
		this.hasSettingsTab = hasSettingsTab;
		this.swapToolOnKeyPress = swapToolOnKeyPress;
	}

	@Override
	public UpgradeType<ToolSwapperUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean hasSettingsTab() {
		return hasSettingsTab;
	}

	public boolean shouldSwapToolOnKeyPress() {
		return swapToolOnKeyPress;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, ItemStack upgradeStack, boolean firstLevelBackpack) {
		Set<Integer> errorUpgradeSlots = new HashSet<>();
		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof ToolSwapperUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});
		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("add.tool_swapper_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}
		return new UpgradeSlotChangeResult.Success();
	}
}
