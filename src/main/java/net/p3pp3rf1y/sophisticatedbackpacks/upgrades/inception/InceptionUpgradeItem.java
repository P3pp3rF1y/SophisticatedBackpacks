package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

public class InceptionUpgradeItem extends UpgradeItemBase<InceptionUpgradeWrapper> {
	public static final UpgradeType<InceptionUpgradeWrapper> TYPE = new UpgradeType<>(InceptionUpgradeWrapper::new);

	public InceptionUpgradeItem() {
		super(SophisticatedBackpacks.ITEM_GROUP, Config.SERVER.maxUpgradesPerStorage);
	}

	@Override
	public UpgradeType<InceptionUpgradeWrapper> getType() {
		return TYPE;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IStorageWrapper storageWrapper, ItemStack upgradeStack, boolean firstLevelStorage, boolean isClientSide) {
		UpgradeSlotChangeResult result = super.canAddUpgradeTo(storageWrapper, upgradeStack, firstLevelStorage, isClientSide);
		if (!result.isSuccessful()) {
			return result;
		}

		if (!firstLevelStorage) {
			return new UpgradeSlotChangeResult.Fail(SBPTranslationHelper.INSTANCE.translError("add.inception_sub_backpack"), Collections.emptySet(), Collections.emptySet(), Collections.emptySet());
		}

		Set<Integer> errorUpgradeSlots = new HashSet<>();
		storageWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof InceptionUpgradeWrapper) {
				errorUpgradeSlots.add(slot);
			}
		});

		if (!errorUpgradeSlots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(SBPTranslationHelper.INSTANCE.translError("add.inception_exists"), errorUpgradeSlots, Collections.emptySet(), Collections.emptySet());
		}

		return new UpgradeSlotChangeResult.Success();
	}

	@Override
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IStorageWrapper storageWrapper, boolean isClientSide) {
		Set<Integer> slots = InventoryHelper.getItemSlots(storageWrapper.getInventoryHandler(), stack -> stack.getItem() instanceof BackpackItem);
		if (!slots.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(SBPTranslationHelper.INSTANCE.translError("remove.inception_sub_backpack"), Collections.emptySet(), slots, Collections.emptySet());
		}
		return new UpgradeSlotChangeResult.Success();
	}
}
