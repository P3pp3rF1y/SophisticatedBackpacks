package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeWrapper> {
	public static final UpgradeType<InceptionUpgradeWrapper> TYPE = new UpgradeType<>(InceptionUpgradeWrapper::new);

	@Override
	public UpgradeType<InceptionUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, ItemStack upgradeStack, boolean firstLevelBackpack) {
		if (!firstLevelBackpack) {
			return new UpgradeSlotChangeResult.Fail(translError("add.inception_sub_backpack"), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		Set<Integer> errorUpgradeSlots = new HashSet<>();
		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof InceptionUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("add.inception_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}

	@Override
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		Set<Integer> slots = InventoryHelper.getItemSlots(backpackWrapper.getInventoryHandler(), stack -> stack.getItem() instanceof BackpackItem);
		if (!slots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("remove.inception_sub_backpack"), Collections.emptySet(), slots, Collections.emptySet());
		}
		return new UpgradeSlotChangeResult.Success();
	}
}
