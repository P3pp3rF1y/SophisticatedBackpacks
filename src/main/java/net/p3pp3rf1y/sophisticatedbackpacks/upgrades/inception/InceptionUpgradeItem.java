package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeWrapper> {
	public static final UpgradeType<InceptionUpgradeWrapper> TYPE = new UpgradeType<>(InceptionUpgradeWrapper::new);
	public static final List<UpgradeConflictDefinition> UPGRADE_CONFLICT_DEFINITIONS = List.of(
			new UpgradeConflictDefinition(InceptionUpgradeItem.class::isInstance, 0, BackpackTranslationHelper.INSTANCE.translError("add.inception_exists")));

	public InceptionUpgradeItem(Properties properties) {
		super(Config.SERVER.maxUpgradesPerStorage, properties);
	}

	@Override
	public UpgradeType<InceptionUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IStorageWrapper storageWrapper, ItemStack upgradeStack, boolean firstLevelStorage, boolean isClientSide) {
		UpgradeSlotChangeResult result = super.canAddUpgradeTo(storageWrapper, upgradeStack, firstLevelStorage, isClientSide);
		if (!result.successful()) {
			return result;
		}

		if (!firstLevelStorage) {
			return UpgradeSlotChangeResult.fail(BackpackTranslationHelper.INSTANCE.translError("add.inception_sub_backpack"), Collections.emptySet(),
					Collections.emptySet(), Collections.emptySet());
		}

		Set<Integer> errorUpgradeSlots = new HashSet<>();
		storageWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof InceptionUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (!errorUpgradeSlots.isEmpty()) {
			return UpgradeSlotChangeResult.fail(BackpackTranslationHelper.INSTANCE.translError("add.inception_exists"), errorUpgradeSlots,
					Collections.emptySet(), Collections.emptySet());
		}

		return UpgradeSlotChangeResult.success();
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return UPGRADE_CONFLICT_DEFINITIONS;
	}

	@Override
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IStorageWrapper storageWrapper, boolean isClientSide) {
		Set<Integer> slots = InventoryHelper.getItemSlots(storageWrapper.getInventoryHandler(), stack -> stack.getItem() instanceof BackpackItem);
		if (!slots.isEmpty()) {
			return UpgradeSlotChangeResult.fail(BackpackTranslationHelper.INSTANCE.translError("remove.inception_sub_backpack"), Collections.emptySet(), slots,
					Collections.emptySet());
		}
		return UpgradeSlotChangeResult.success();
	}

	@Override
	public UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, int upgradeSlot, IStorageWrapper storageWrapper, boolean isClientSide) {
		if (upgradeStackToPut.getItem() == this) {
			return UpgradeSlotChangeResult.success();
		}

		return canRemoveUpgradeFrom(storageWrapper, isClientSide);
	}
}
