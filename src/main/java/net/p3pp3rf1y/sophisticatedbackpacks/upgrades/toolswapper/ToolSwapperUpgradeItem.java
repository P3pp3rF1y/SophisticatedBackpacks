package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class ToolSwapperUpgradeItem extends UpgradeItemBase<ToolSwapperUpgradeWrapper> {
	private static final UpgradeType<ToolSwapperUpgradeWrapper> TYPE = new UpgradeType<>(ToolSwapperUpgradeWrapper::new);
	private final boolean hasSettingsTab;
	private final boolean swapToolOnKeyPress;

	public ToolSwapperUpgradeItem(boolean hasSettingsTab, boolean swapToolOnKeyPress) {
		super(SophisticatedBackpacks.ITEM_GROUP, Config.SERVER.maxUpgradesPerStorage);
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
	public UpgradeSlotChangeResult canAddUpgradeTo(IStorageWrapper storageWrapper, ItemStack upgradeStack, boolean firstLevelStorage, boolean isClientSide) {
		UpgradeSlotChangeResult result = super.canAddUpgradeTo(storageWrapper, upgradeStack, firstLevelStorage, isClientSide);
		if (!result.isSuccessful()) {
			return result;
		}

		Set<Integer> errorUpgradeSlots = new HashSet<>();
		storageWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof ToolSwapperUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});
		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(SBPTranslationHelper.INSTANCE.translError("add.tool_swapper_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}
		return new UpgradeSlotChangeResult.Success();
	}
}
