package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stack;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IStackableContentsUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

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
	public UpgradeSlotChangeResult canRemoveUpgradeFrom(IBackpackWrapper backpackWrapper) {
		int currentInventoryMultiplier = getInventorySlotLimit(backpackWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;
		return isMultiplierHighEnough(backpackWrapper, multiplierWhenRemoved);
	}

	@Override
	public UpgradeSlotChangeResult canSwapUpgradeFor(ItemStack upgradeStackToPut, IBackpackWrapper backpackWrapper) {
		if (!(upgradeStackToPut.getItem() instanceof StackUpgradeItem)) {
			return canRemoveUpgradeFrom(backpackWrapper);
		}

		StackUpgradeItem otherStackUpgradeItem = (StackUpgradeItem) upgradeStackToPut.getItem();
		if (otherStackUpgradeItem.stackSizeMultiplier >= stackSizeMultiplier) {
			return new UpgradeSlotChangeResult.Success();
		}

		int currentInventoryMultiplier = getInventorySlotLimit(backpackWrapper) / 64;
		int multiplierWhenRemoved = currentInventoryMultiplier / stackSizeMultiplier;

		return isMultiplierHighEnough(backpackWrapper, multiplierWhenRemoved * otherStackUpgradeItem.stackSizeMultiplier);
	}

	private UpgradeSlotChangeResult isMultiplierHighEnough(IBackpackWrapper backpackWrapper, int multiplier) {
		Set<Integer> slotsOverMultiplier = new HashSet<>();

		InventoryHelper.iterate(backpackWrapper.getInventoryHandler(), (slot, stack) -> {
			int stackMultiplierNeeded = (stack.getCount() / stack.getMaxStackSize()) + (stack.getCount() % stack.getMaxStackSize() != 0 ? 1 : 0);
			if (stackMultiplierNeeded > multiplier) {
				slotsOverMultiplier.add(slot);
			}
		});

		Set<Integer> errorInventoryParts = new HashSet<>();

		backpackWrapper.getUpgradeHandler().getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof IStackableContentsUpgrade && ((IStackableContentsUpgrade) wrapper).getMinimumMultiplierRequired() > multiplier) {
				errorInventoryParts.add(slot);
			}
		});

		if (!slotsOverMultiplier.isEmpty() || !errorInventoryParts.isEmpty()) {
			return new UpgradeSlotChangeResult.Fail(translError("remove.stack_low_multiplier", multiplier), Collections.emptySet(), slotsOverMultiplier, errorInventoryParts);
		}

		return new UpgradeSlotChangeResult.Success();
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
