package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.furnace;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class FurnaceUpgradeContainer extends UpgradeContainerBase<FurnaceUpgradeWrapper, FurnaceUpgradeContainer> {
	public static final UpgradeContainerType<FurnaceUpgradeWrapper, FurnaceUpgradeContainer> TYPE = new UpgradeContainerType<>(FurnaceUpgradeContainer::new);

	public FurnaceUpgradeContainer(int containerId, FurnaceUpgradeWrapper wrapper, boolean isClientSide, UpgradeContainerType<FurnaceUpgradeWrapper, FurnaceUpgradeContainer> type) {
		super(containerId, wrapper, isClientSide, type);
		slots.add(new SlotSuppliedHandler(() -> upgradeWrapper.getFurnaceInventory(), FurnaceUpgradeWrapper.COOK_INPUT_SLOT, -100, -100));
		slots.add(new SlotSuppliedHandler(() -> upgradeWrapper.getFurnaceInventory(), FurnaceUpgradeWrapper.FUEL_SLOT, -100, -100));
		slots.add(new SlotSuppliedHandler(() -> upgradeWrapper.getFurnaceInventory(), FurnaceUpgradeWrapper.COOK_OUTPUT_SLOT, -100, -100) {
			@Override
			public boolean isItemValid(ItemStack stack) {
				return false; //needs to not allow player putting anything in
			}
		});
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		//noop
	}

	public int getBurnTimeTotal() {
		return upgradeWrapper.getBurnTimeTotal();
	}

	public long getBurnTimeFinish() {
		return upgradeWrapper.getBurnTimeFinish();
	}

	public long getCookTimeFinish() {
		return upgradeWrapper.getCookTimeFinish();
	}

	public int getCookTimeTotal() {
		return upgradeWrapper.getCookTimeTotal();
	}

	public boolean isCooking() {
		return upgradeWrapper.isCooking();
	}

	public boolean isBurning(World world) {
		return upgradeWrapper.isBurning(world);
	}
}
