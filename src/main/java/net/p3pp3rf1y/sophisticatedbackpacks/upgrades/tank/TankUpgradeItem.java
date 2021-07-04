package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.fluid.Fluids;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeSlotChangeResult;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import java.util.Collections;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.utils.TranslationHelper.translError;

public class TankUpgradeItem extends UpgradeItemBase<TankUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	@Override
	public UpgradeSlotChangeResult canAddUpgradeTo(IBackpackWrapper backpackWrapper, boolean firstLevelBackpack) {
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
			return new UpgradeSlotChangeResult.Fail(translError("add.tank_slots_not_empty"), Collections.emptySet(), errorSlots);
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

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, TankUpgradeItem> implements IRenderedTankUpgrade {
		private Consumer<TankRenderInfo> updateTankRenderInfoCallback;

		protected Wrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
		}

		@Override
		public void setTankRenderInfoUpdateCallback(Consumer<TankRenderInfo> updateTankRenderInfoCallback) {
			this.updateTankRenderInfoCallback = updateTankRenderInfoCallback;
		}

		@Override
		public void forceUpdateTankRenderInfo() {
			TankRenderInfo renderInfo = new TankRenderInfo();
			renderInfo.setFluid(Fluids.LAVA);
			renderInfo.setFillRatio(0.5f);
			updateTankRenderInfoCallback.accept(renderInfo);
		}
	}
}
