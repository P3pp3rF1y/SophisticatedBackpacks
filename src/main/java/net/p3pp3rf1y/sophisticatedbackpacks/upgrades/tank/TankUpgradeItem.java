package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stack.StackUpgradeItem;

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

		int numberOfRows;
		int slots = backpackWrapper.getInventoryHandler().getSlots();
		numberOfRows = getNumberOfRows(slots);

		BackpackInventoryHandler invHandler = backpackWrapper.getInventoryHandler();
		Set<Integer> errorSlots = new HashSet<>();
		for (int slot = slots - 1; slot >= slots - 2 * numberOfRows; slot--) {
			if (!invHandler.getStackInSlot(slot).isEmpty()) {
				errorSlots.add(slot);
			}
		}

		if (!errorSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("add.needs_occupied_inventory_slots", 2 * numberOfRows, new ItemStack(this).getDisplayName()), Collections.emptySet(), errorSlots, Collections.emptySet());
		}

		int backpackStackMultiplier = StackUpgradeItem.getInventorySlotLimit(backpackWrapper) / 64;
		int multiplierRequired = (int) Math.ceil((float) TankUpgradeWrapper.getContents(upgradeStack).getAmount() / (Config.COMMON.tankUpgrade.capacityPerSlotRow.get() * backpackWrapper.getNumberOfSlotRows() * backpackStackMultiplier));
		if (multiplierRequired / backpackStackMultiplier > 1) {
			return new UpgradeSlotChangeResult.Fail(translError("add.tank_capacity_high", multiplierRequired), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}

	private int getNumberOfRows(int slots) {
		int slotsOnLine = slots > 81 ? 12 : 9;
		return (int) Math.ceil((double) slots / slotsOnLine);
	}

	@Override
	public int getInventoryColumnsTaken() {
		return 2;
	}

}
