package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stack;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

public class StackUpgradeItem extends UpgradeItemBase<StackUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);
	private final int stackSizeMultiplier;

	public StackUpgradeItem(int stackSizeMultiplier) {
		super();
		this.stackSizeMultiplier = stackSizeMultiplier;
	}

	public static int getInventorySlotLimit(IBackpackWrapper backpackWrapper) {
		int multiplier = 1;

		for (Wrapper stackWrapper : backpackWrapper.getUpgradeHandler().getTypeWrappers(TYPE)) {
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
	public boolean canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		int currentInventoryMultiplier = getInventorySlotLimit(backpackWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;
		return isMultiplierHighEnough(backpackWrapper, multiplierWhenRemoved);
	}

	@Override
	public boolean canSwapUpgradeFor(ItemStack upgradeStackToPut, IBackpackWrapper backpackWrapper) {
		if (!(upgradeStackToPut.getItem() instanceof StackUpgradeItem)) {
			return canRemoveUpgradeFrom(backpackWrapper);
		}

		StackUpgradeItem otherStackUpgradeItem = (StackUpgradeItem) upgradeStackToPut.getItem();
		if (otherStackUpgradeItem.stackSizeMultiplier >= stackSizeMultiplier) {
			return true;
		}

		int currentInventoryMultiplier = getInventorySlotLimit(backpackWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;

		return isMultiplierHighEnough(backpackWrapper, multiplierWhenRemoved * otherStackUpgradeItem.stackSizeMultiplier);
	}

	private boolean isMultiplierHighEnough(IBackpackWrapper backpackWrapper, int multiplier) {
		AtomicInteger maxMultiplierNeeded = new AtomicInteger(0);

		InventoryHelper.iterate(backpackWrapper.getInventoryHandler(), (slot, stack) -> {
			int stackMultiplierNeeded = (stack.getCount() / stack.getMaxStackSize()) + (stack.getCount() % stack.getMaxStackSize() != 0 ? 1 : 0);
			if (stackMultiplierNeeded > maxMultiplierNeeded.get()) {
				maxMultiplierNeeded.set(stackMultiplierNeeded);
			}
		}, () -> multiplier < maxMultiplierNeeded.get());

		return multiplier >= maxMultiplierNeeded.get();
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, StackUpgradeItem> {
		protected Wrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
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
