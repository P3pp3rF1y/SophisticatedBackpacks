package net.p3pp3rf1y.sophisticatedcore.upgrades.battery;

import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class BatteryUpgradeItem extends UpgradeItemBase<BatteryUpgradeWrapper> {
	public static final UpgradeType<BatteryUpgradeWrapper> TYPE = new UpgradeType<>(BatteryUpgradeWrapper::new);

	private final BatteryUpgradeConfig batteryUpgradeConfig;

	public BatteryUpgradeItem(CreativeModeTab itemGroup, BatteryUpgradeConfig batteryUpgradeConfig) {
		super(itemGroup);
		this.batteryUpgradeConfig = batteryUpgradeConfig;
	}

	public BatteryUpgradeConfig getBatteryUpgradeConfig() {
		return batteryUpgradeConfig;
	}

	@Override
	public UpgradeType<BatteryUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public int getInventoryColumnsTaken() {
		return 2;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IStorageWrapper storageWrapper, ItemStack upgradeStack, boolean firstLevelStorage) {
		Set<Integer> errorUpgradeSlots = new HashSet<>();
		storageWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof BatteryUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(TranslationHelper.INSTANCE.translError("add.battery_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}

		int multiplierRequired = (int) Math.ceil((float) BatteryUpgradeWrapper.getEnergyStored(upgradeStack) / getMaxEnergyStored(storageWrapper));
		if (multiplierRequired > 1) {
			return new UpgradeSlotChangeResult.Fail(TranslationHelper.INSTANCE.translError("add.battery_energy_high", multiplierRequired), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}

	public int getMaxEnergyStored(IStorageWrapper storageWrapper) {
		int stackMultiplier = getAdjustedStackMultiplier(storageWrapper);
		int maxEnergyBase = getMaxEnergyBase(storageWrapper);
		return Integer.MAX_VALUE / stackMultiplier < maxEnergyBase ? Integer.MAX_VALUE : maxEnergyBase * stackMultiplier;
	}

	public int getAdjustedStackMultiplier(IStorageWrapper storageWrapper) {
		return 1 + (int) (batteryUpgradeConfig.stackMultiplierRatio.get() * (storageWrapper.getInventoryHandler().getStackSizeMultiplier() - 1));
	}

	public int getMaxEnergyBase(IStorageWrapper storageWrapper) {
		return batteryUpgradeConfig.energyPerSlotRow.get() * storageWrapper.getNumberOfSlotRows();
	}
}
