package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.network.chat.Component;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeCountLimitConfig;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;

import java.util.List;
import java.util.Set;

public class MobCatcherUpgradeItem extends UpgradeItemBase<MobCatcherUpgradeWrapper> {
	public static final UpgradeType<MobCatcherUpgradeWrapper> TYPE = new UpgradeType<>(MobCatcherUpgradeWrapper::new);
	private final boolean advanced;

	public MobCatcherUpgradeItem(boolean advanced, IUpgradeCountLimitConfig upgradeTypeLimitConfig, Properties properties) {
		super(upgradeTypeLimitConfig, properties);
		this.advanced = advanced;
	}

	@Override
	public UpgradeType<MobCatcherUpgradeWrapper> getType() {
		return TYPE;
	}

	public boolean isAdvanced() {
		return advanced;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return List.of(new UpgradeConflictDefinition(item -> item instanceof MobCatcherUpgradeItem, 0,
				Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_only_one_allowed")));
	}

	@Override
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IStorageWrapper storageWrapper, boolean isClientSide) {
		if (!(storageWrapper instanceof IBackpackWrapper backpackWrapper)) {
			return UpgradeSlotChangeResult.success();
		}
		return MobCatcherStorage.getCapturedMobs(backpackWrapper).isEmpty() ? UpgradeSlotChangeResult.success() : UpgradeSlotChangeResult.fail(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_contains_mobs"), Set.of(), Set.of(), Set.of());
	}

	@Override
	public UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, int upgradeSlot, IStorageWrapper storageWrapper, boolean isClientSide) {
		if (!(storageWrapper instanceof IBackpackWrapper backpackWrapper)) {
			return UpgradeSlotChangeResult.success();
		}
		if (upgradeStackToPut.getItem() instanceof MobCatcherUpgradeItem mobCatcherUpgradeItem && mobCatcherUpgradeItem.isAdvanced()) {
			return UpgradeSlotChangeResult.success();
		}
		return MobCatcherStorage.canFitBasicTier(backpackWrapper, Config.SERVER.mobCatcherUpgrade.basicMaxSlotCost.get()) ? UpgradeSlotChangeResult.success() : UpgradeSlotChangeResult.fail(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_mobs_need_advanced"), Set.of(), Set.of(), Set.of());
	}
}
