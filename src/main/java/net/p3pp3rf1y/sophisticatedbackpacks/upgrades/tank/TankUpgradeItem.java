package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

public class TankUpgradeItem extends UpgradeItemBase<TankUpgradeWrapper> {
	public static final UpgradeType<TankUpgradeWrapper> TYPE = new UpgradeType<>(TankUpgradeWrapper::new);

	@Override
	public UpgradeType<TankUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, ItemStack upgradeStack, boolean firstLevelBackpack) {
		Set<Integer> errorUpgradeSlots = new HashSet<>();
		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof TankUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (errorUpgradeSlots.size() >= 2) {
			return new UpgradeSlotChangeResult.Fail(translError("add.two_tank_upgrades_present"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}

		int multiplierRequired = (int) Math.ceil((float) TankUpgradeWrapper.getContents(upgradeStack).getAmount() / TankUpgradeWrapper.getTankCapacity(backpackWrapper));
		if (multiplierRequired > 1) {
			return new UpgradeSlotChangeResult.Fail(translError("add.tank_capacity_high", multiplierRequired), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}

	@Override
	public int getInventoryColumnsTaken() {
		return 2;
	}
}
