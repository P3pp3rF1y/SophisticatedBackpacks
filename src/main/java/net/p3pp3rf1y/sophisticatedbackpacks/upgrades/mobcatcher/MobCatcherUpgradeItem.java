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

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class MobCatcherUpgradeItem extends UpgradeItemBase<MobCatcherUpgradeWrapper> {
	public static final UpgradeType<MobCatcherUpgradeWrapper> TYPE = new UpgradeType<>(MobCatcherUpgradeWrapper::new);
	private final boolean advanced;

	public MobCatcherUpgradeItem(boolean advanced, IUpgradeCountLimitConfig upgradeTypeLimitConfig) {
		super(upgradeTypeLimitConfig);
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
		List<CapturedMob> capturedMobs = MobCatcherStorage.getCapturedMobs(backpackWrapper);
		return capturedMobs.isEmpty() ? UpgradeSlotChangeResult.success() : UpgradeSlotChangeResult.fail(Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_contains_mobs"),
				getMobCatcherUpgradeSlots(backpackWrapper), getOccupiedCapturedMobSlots(backpackWrapper, capturedMobs), Set.of());
	}

	@Override
	public UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, int upgradeSlot, IStorageWrapper storageWrapper, boolean isClientSide) {
		if (!(storageWrapper instanceof IBackpackWrapper backpackWrapper)) {
			return UpgradeSlotChangeResult.success();
		}
		if (upgradeStackToPut.getItem() instanceof MobCatcherUpgradeItem mobCatcherUpgradeItem && mobCatcherUpgradeItem.isAdvanced()) {
			return UpgradeSlotChangeResult.success();
		}
		List<CapturedMob> capturedMobs = MobCatcherStorage.getCapturedMobs(backpackWrapper);
		return MobCatcherStorage.canFitBasicTier(backpackWrapper, Config.SERVER.mobCatcherUpgrade.basicMaxSlotCost.get()) ? UpgradeSlotChangeResult.success() : UpgradeSlotChangeResult.fail(
				Component.translatable("gui.sophisticatedbackpacks.status.mob_catcher_mobs_need_advanced"), Set.of(upgradeSlot), getOccupiedCapturedMobSlots(backpackWrapper, capturedMobs), Set.of());
	}

	private Set<Integer> getMobCatcherUpgradeSlots(IBackpackWrapper backpackWrapper) {
		Set<Integer> slots = new HashSet<>();
		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof MobCatcherUpgradeWrapper) {
				slots.add(slot);
			}
		});
		return slots;
	}

	private Set<Integer> getOccupiedCapturedMobSlots(IBackpackWrapper backpackWrapper, List<CapturedMob> capturedMobs) {
		Set<Integer> slots = new HashSet<>();
		int columns = MobCatcherStorage.getColumns(backpackWrapper);
		for (CapturedMob capturedMob : capturedMobs) {
			for (int yOffset = 0; yOffset < capturedMob.height(); yOffset++) {
				for (int xOffset = 0; xOffset < capturedMob.width(); xOffset++) {
					slots.add(capturedMob.slot() + yOffset * columns + xOffset);
				}
			}
		}
		return slots;
	}
}
