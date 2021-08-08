package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

public class BatteryUpgradeItem extends UpgradeItemBase<BatteryUpgradeWrapper> {
	public static final UpgradeType<BatteryUpgradeWrapper> TYPE = new UpgradeType<>(BatteryUpgradeWrapper::new);

	@Override
	public UpgradeType<BatteryUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public int getInventoryColumnsTaken() {
		return 2;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, ItemStack upgradeStack, boolean firstLevelBackpack) {
		Set<Integer> errorUpgradeSlots = new HashSet<>();
		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof BatteryUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("add.battery_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}

		int multiplierRequired = (int) Math.ceil((float) BatteryUpgradeWrapper.getEnergyStored(upgradeStack) / BatteryUpgradeWrapper.getMaxEnergyStored(backpackWrapper));
		if (multiplierRequired > 1) {
			return new UpgradeSlotChangeResult.Fail(translError("add.battery_energy_high", multiplierRequired), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}
}
