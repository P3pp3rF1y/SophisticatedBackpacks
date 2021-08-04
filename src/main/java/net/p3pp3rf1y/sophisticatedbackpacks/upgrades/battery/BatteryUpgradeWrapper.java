package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.battery;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.energy.CapabilityEnergy;
import net.minecraftforge.energy.IEnergyStorage;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.function.Consumer;

public class BatteryUpgradeWrapper extends UpgradeWrapperBase<BatteryUpgradeWrapper, BatteryUpgradeItem>
		implements IRenderedBatteryUpgrade, IEnergyStorage, ITickableUpgrade {
	public static final int INPUT_SLOT = 0;
	public static final int OUTPUT_SLOT = 1;
	public static final String ENERGY_STORED_TAG = "energyStored";
	private Consumer<BatteryRenderInfo> updateTankRenderInfoCallback;
	private final ItemStackHandler inventory;
	private int energyStored;

	private int outputThisTick = 0;
	private int inputThisTick = 0;

	protected BatteryUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		inventory = new ItemStackHandler(2) {
			@Override
			protected void onContentsChanged(int slot) {
				super.onContentsChanged(slot);
				upgrade.addTagElement("inventory", serializeNBT());
				save();
			}

			@Override
			public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
				if (slot == INPUT_SLOT) {
					return isValidInputItem(stack);
				} else if (slot == OUTPUT_SLOT) {
					return isValidOutputItem(stack);
				}
				return false;
			}

			private boolean isValidInputItem(ItemStack stack) {
				return isValidEnergyItem(stack, false);
			}

			private boolean isValidOutputItem(ItemStack stack) {
				return isValidEnergyItem(stack, true);
			}

		};
		energyStored = getEnergyStored(upgrade);
	}

	public static int getEnergyStored(ItemStack upgrade) {
		return NBTHelper.getInt(upgrade, ENERGY_STORED_TAG).orElse(0);
	}

	@Override
	public int receiveEnergy(int maxReceive, boolean simulate) {
		if (inputThisTick >= getMaxInOut()) {
			return 0;
		}

		int ret = Math.min(getMaxEnergyStored() - energyStored, Math.min(getMaxInOut() - inputThisTick, maxReceive));
		if (!simulate) {
			energyStored += ret;
			NBTHelper.setInteger(upgrade, ENERGY_STORED_TAG, energyStored);
			save();
			inputThisTick += ret;
		}
		return ret;
	}

	@Override
	public int extractEnergy(int maxExtract, boolean simulate) {
		if (outputThisTick > getMaxInOut()) {
			return 0;
		}

		int ret = Math.min(energyStored, Math.min(getMaxInOut() - outputThisTick, maxExtract));

		if (!simulate) {
			energyStored -= ret;
			NBTHelper.setInteger(upgrade, ENERGY_STORED_TAG, energyStored);
			save();
			outputThisTick += ret;
		}
		return ret;
	}

	@Override
	public int getEnergyStored() {
		return energyStored;
	}

	@Override
	public int getMaxEnergyStored() {
		int stackMultiplier = getAdjustedStackMultiplier(backpackWrapper);
		return getMaxEnergyBase() * stackMultiplier;
	}

	@Override
	public boolean canExtract() {
		return true;
	}

	@Override
	public boolean canReceive() {
		return true;
	}

	public static int getAdjustedStackMultiplier(IBackpackWrapper backpackWrapper) {
		return 1 + (int) (Config.COMMON.batteryUpgrade.stackMultiplierRatio.get() * (backpackWrapper.getInventoryHandler().getStackSizeMultiplier() - 1));
	}

	private int getMaxEnergyBase() {
		return Config.COMMON.batteryUpgrade.energyPerSlotRow.get() * backpackWrapper.getNumberOfSlotRows();
	}

	private int getMaxInOut() {
		return Config.COMMON.batteryUpgrade.maxInputOutput.get() * backpackWrapper.getNumberOfSlotRows() * getAdjustedStackMultiplier(backpackWrapper);
	}

	private boolean isValidEnergyItem(ItemStack stack, boolean isOutput) {
		return stack.getCapability(CapabilityEnergy.ENERGY).map(energyStorage -> isOutput || energyStorage.getEnergyStored() > 0).orElse(false);
	}

	@Override
	public void setBatteryRenderInfoUpdateCallback(Consumer<BatteryRenderInfo> updateTankRenderInfoCallback) {
		this.updateTankRenderInfoCallback = updateTankRenderInfoCallback;
	}

	@Override
	public void forceUpdateBatteryRenderInfo() {
		updateTankRenderInfoCallback.accept(new BatteryRenderInfo(1f));
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		inputThisTick = 0;
		outputThisTick = 0;

		if (energyStored < getMaxEnergyStored()) {
			inventory.getStackInSlot(INPUT_SLOT).getCapability(CapabilityEnergy.ENERGY).ifPresent(this::receiveFromStorage);
		}

		if (energyStored > 0) {
			inventory.getStackInSlot(OUTPUT_SLOT).getCapability(CapabilityEnergy.ENERGY).ifPresent(this::extractToStorage);
		}
	}

	private void extractToStorage(IEnergyStorage energyStorage) {
		int toExtract = extractEnergy(getMaxInOut(), true);
		if (toExtract > 0) {
			toExtract = energyStorage.receiveEnergy(toExtract, true);
			if (toExtract > 0) {
				energyStorage.receiveEnergy(toExtract, false);
				extractEnergy(toExtract, false);
			}
		}
	}

	private void receiveFromStorage(IEnergyStorage energyStorage) {
		int toReceive = receiveEnergy(getMaxInOut(), true);
		if (toReceive > 0) {
			toReceive = energyStorage.extractEnergy(toReceive, true);
			if (toReceive > 0) {
				energyStorage.extractEnergy(toReceive, false);
				receiveEnergy(toReceive, false);
			}
		}
	}

	public IItemHandler getInventory() {
		return inventory;
	}
}
