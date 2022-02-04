package net.p3pp3rf1y.sophisticatedcore.upgrades.stack;

import net.minecraft.world.item.CreativeModeTab;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IStackableContentsUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

public class StackUpgradeItem extends UpgradeItemBase<StackUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);
	private final int stackSizeMultiplier;

	public StackUpgradeItem(int stackSizeMultiplier, CreativeModeTab itemGroup) {
		super(itemGroup);
		this.stackSizeMultiplier = stackSizeMultiplier;
	}

	public static int getInventorySlotLimit(IStorageWrapper storageWrapper) {
		int multiplier = 1;

		for (Wrapper stackWrapper : storageWrapper.getUpgradeHandler().getTypeWrappers(TYPE)) {
			if (Integer.MAX_VALUE / stackWrapper.getStackSizeMultiplier() < multiplier) {
				return Integer.MAX_VALUE;
			}
			multiplier *= stackWrapper.getStackSizeMultiplier();
		}

		return Integer.MAX_VALUE / 64 < multiplier ? Integer.MAX_VALUE : multiplier * 64;
	}

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	int getStackSizeMultiplier() {
		return stackSizeMultiplier;
	}

	@Override
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IStorageWrapper storageWrapper) {
		int currentInventoryMultiplier = getInventorySlotLimit(storageWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;
		return isMultiplierHighEnough(storageWrapper, multiplierWhenRemoved);
	}

	@Override
	public UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, IStorageWrapper storageWrapper) {
		if (!(upgradeStackToPut.getItem() instanceof StackUpgradeItem otherStackUpgradeItem)) {
			return canRemoveUpgradeFrom(storageWrapper);
		}

		if (otherStackUpgradeItem.stackSizeMultiplier >= stackSizeMultiplier) {
			return new UpgradeSlotChangeResult.Success();
		}

		int currentInventoryMultiplier = getInventorySlotLimit(storageWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;

		return isMultiplierHighEnough(storageWrapper, multiplierWhenRemoved * otherStackUpgradeItem.stackSizeMultiplier);
	}

	private UpgradeSlotChangeResult isMultiplierHighEnough(IStorageWrapper storageWrapper, int multiplier) {
		Set<Integer> slotsOverMultiplier = new HashSet<>();

		InventoryHelper.iterate(storageWrapper.getInventoryHandler(), (slot, stack) -> {
			int stackMultiplierNeeded = (stack.getCount() / stack.getMaxStackSize()) + (stack.getCount() % stack.getMaxStackSize() != 0 ? 1 : 0);
			if (stackMultiplierNeeded > multiplier) {
				slotsOverMultiplier.add(slot);
			}
		});

		Set<Integer> errorInventoryParts = new HashSet<>();

		storageWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof IStackableContentsUpgrade stackableContentsUpgrade && stackableContentsUpgrade.getMinimumMultiplierRequired() > multiplier) {
				errorInventoryParts.add(slot);
			}
		});

		if (!slotsOverMultiplier.isEmpty() || !errorInventoryParts.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(TranslationHelper.INSTANCE.translError("remove.stack_low_multiplier", multiplier), Collections.emptySet(), slotsOverMultiplier, errorInventoryParts);
		}

		return new UpgradeSlotChangeResult.Success();
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, StackUpgradeItem> {
		protected Wrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(storageWrapper, upgrade, upgradeSaveHandler);
		}

		public int getStackSizeMultiplier() {
			return upgradeItem.getStackSizeMultiplier();
		}

		@Override
		public boolean hideSettingsTab() {
			return true;
		}

		@Override
		public boolean canBeDisabled() {
			return false;
		}
	}
}
