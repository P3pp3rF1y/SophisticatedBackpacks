package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.entity.LivingEntity;
import net.minecraft.fluid.Fluid;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.fluids.FluidAttributes;
import net.minecraftforge.fluids.FluidStack;
import net.minecraftforge.fluids.capability.CapabilityFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandler;
import net.minecraftforge.fluids.capability.IFluidHandlerItem;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IStackableContentsUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;

public class TankUpgradeWrapper extends UpgradeWrapperBase<TankUpgradeWrapper, TankUpgradeItem>
		implements IRenderedTankUpgrade, ITickableUpgrade, IStackableContentsUpgrade {
	public static final int INPUT_SLOT = 0;
	public static final int OUTPUT_SLOT = 1;
	private static final String CONTENTS_TAG = "contents";
	private Consumer<TankRenderInfo> updateTankRenderInfoCallback;
	private final ItemStackHandler inventory;
	private FluidStack contents;
	private long cooldownTime = 0;

	protected TankUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
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
				return isValidFluidItem(stack, false);
			}

			private boolean isValidOutputItem(ItemStack stack) {
				return isValidFluidItem(stack, true);
			}

			@Override
			public int getSlotLimit(int slot) {
				return 1;
			}
		};
		NBTHelper.getCompound(upgrade, "inventory").ifPresent(inventory::deserializeNBT);
		contents = getContents(upgrade);
	}

	public static FluidStack getContents(ItemStack upgrade) {
		return NBTHelper.getCompound(upgrade, CONTENTS_TAG).map(FluidStack::loadFluidStackFromNBT).orElse(FluidStack.EMPTY);
	}

	private boolean isValidFluidItem(ItemStack stack, boolean isOutput) {
		return stack.getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).map(fluidHandler ->
				isValidFluidHandler(fluidHandler, isOutput)).orElse(false);
	}

	private boolean isValidFluidHandler(IFluidHandlerItem fluidHandler, boolean isOutput) {
		boolean tankEmpty = contents.isEmpty();
		for (int tank = 0; tank < fluidHandler.getTanks(); tank++) {
			FluidStack fluidInTank = fluidHandler.getFluidInTank(tank);
			if ((isOutput && (fluidInTank.isEmpty() || (!tankEmpty && fluidInTank.getFluid() == contents.getFluid())))
					|| (!isOutput && !fluidInTank.isEmpty() && (tankEmpty || contents.getFluid() == fluidInTank.getFluid()))
			) {
				return true;
			}
		}
		return false;
	}

	@Override
	public void setTankRenderInfoUpdateCallback(Consumer<TankRenderInfo> updateTankRenderInfoCallback) {
		this.updateTankRenderInfoCallback = updateTankRenderInfoCallback;
	}

	@Override
	public void forceUpdateTankRenderInfo() {
		TankRenderInfo renderInfo = new TankRenderInfo();
		renderInfo.setFluid(contents.getFluid());
		renderInfo.setFillRatio((float) Math.round((float) contents.getAmount() / getTankCapacity() * 10) / 10);
		updateTankRenderInfoCallback.accept(renderInfo);
	}

	public FluidStack getContents() {
		return contents;
	}

	public int getTankCapacity() {
		return getTankCapacity(backpackWrapper);
	}

	public static int getAdjustedStackMultiplier(IBackpackWrapper backpackWrapper) {
		return 1 + (int) (Config.COMMON.tankUpgrade.stackMultiplierRatio.get() * (backpackWrapper.getInventoryHandler().getStackSizeMultiplier() - 1));
	}

	private static int getBaseCapacity(IBackpackWrapper backpackWrapper) {
		return Config.COMMON.tankUpgrade.capacityPerSlotRow.get() * backpackWrapper.getNumberOfSlotRows();
	}

	public static int getTankCapacity(IBackpackWrapper backpackWrapper) {
		int stackMultiplier = getAdjustedStackMultiplier(backpackWrapper);
		int baseCapacity = getBaseCapacity(backpackWrapper);
		return Integer.MAX_VALUE / stackMultiplier < baseCapacity ? Integer.MAX_VALUE : baseCapacity * stackMultiplier;
	}

	public IItemHandler getInventory() {
		return inventory;
	}

	private int getMaxInOut() {
		return Math.max(FluidAttributes.BUCKET_VOLUME, Config.COMMON.tankUpgrade.maxInputOutput.get() * backpackWrapper.getNumberOfSlotRows() * getAdjustedStackMultiplier(backpackWrapper));
	}

	public int fill(FluidStack resource, IFluidHandler.FluidAction action) {
		int capacity = getTankCapacity();

		if (contents.getAmount() >= capacity || (!contents.isEmpty() && resource.getFluid() != contents.getFluid())) {
			return 0;
		}

		int toFill = Math.min(getMaxInOut(), Math.min(capacity - contents.getAmount(), resource.getAmount()));

		if (action == IFluidHandler.FluidAction.EXECUTE) {
			if (contents.isEmpty()) {
				contents = new FluidStack(resource.getFluid(), toFill);
			} else {
				contents.setAmount(contents.getAmount() + toFill);
			}
			serializeContents();
		}

		return toFill;
	}

	private void serializeContents() {
		upgrade.addTagElement(CONTENTS_TAG, contents.writeToNBT(new CompoundNBT()));
		save();
		forceUpdateTankRenderInfo();
	}

	public FluidStack drain(int maxDrain, IFluidHandler.FluidAction action) {
		if (contents.isEmpty()) {
			return FluidStack.EMPTY;
		}

		int toDrain = Math.min(getMaxInOut(), Math.min(maxDrain, contents.getAmount()));
		FluidStack ret = new FluidStack(contents.getFluid(), toDrain);
		if (action == IFluidHandler.FluidAction.EXECUTE) {
			if (toDrain == contents.getAmount()) {
				contents = FluidStack.EMPTY;
			} else {
				contents.setAmount(contents.getAmount() - toDrain);
			}
			serializeContents();
		}

		return ret;
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (world.getGameTime() < cooldownTime) {
			return;
		}

		AtomicBoolean didSomething = new AtomicBoolean(false);
		inventory.getStackInSlot(INPUT_SLOT).getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).ifPresent(fluidHandler ->
				didSomething.set(drainHandler(fluidHandler, stack -> inventory.setStackInSlot(INPUT_SLOT, stack)))
		);
		inventory.getStackInSlot(OUTPUT_SLOT).getCapability(CapabilityFluidHandler.FLUID_HANDLER_ITEM_CAPABILITY).ifPresent(fluidHandler ->
				didSomething.set(fillHandler(fluidHandler, stack -> inventory.setStackInSlot(OUTPUT_SLOT, stack)))
		);

		if (didSomething.get()) {
			cooldownTime = world.getGameTime() + Config.COMMON.tankUpgrade.autoFillDrainContainerCooldown.get();
		}
	}

	public boolean fillHandler(IFluidHandlerItem fluidHandler, Consumer<ItemStack> updateContainerStack) {
		if (!contents.isEmpty() && isValidFluidHandler(fluidHandler, true)) {
			Fluid fluid = contents.getFluid();
			int filled = fluidHandler.fill(new FluidStack(fluid, Math.min(FluidAttributes.BUCKET_VOLUME, contents.getAmount())), IFluidHandler.FluidAction.SIMULATE);
			if (filled == 0) {
				return false;
			}
			FluidStack drained = drain(filled, IFluidHandler.FluidAction.EXECUTE);
			fluidHandler.fill(drained, IFluidHandler.FluidAction.EXECUTE);
			updateContainerStack.accept(fluidHandler.getContainer());
			return true;
		}
		return false;
	}

	public boolean drainHandler(IFluidHandlerItem fluidHandler, Consumer<ItemStack> updateContainerStack) {
		if (isValidFluidHandler(fluidHandler, false)) {
			Fluid fluid = contents.getFluid();
			FluidStack extracted = contents.isEmpty() ?
					fluidHandler.drain(FluidAttributes.BUCKET_VOLUME, IFluidHandler.FluidAction.SIMULATE) :
					fluidHandler.drain(new FluidStack(fluid, Math.min(FluidAttributes.BUCKET_VOLUME, getTankCapacity() - contents.getAmount())), IFluidHandler.FluidAction.SIMULATE);
			if (extracted.isEmpty()) {
				return false;
			}
			int filled = fill(extracted, IFluidHandler.FluidAction.EXECUTE);
			FluidStack toExtract = filled == extracted.getAmount() ? extracted : new FluidStack(extracted.getFluid(), filled);
			fluidHandler.drain(toExtract, IFluidHandler.FluidAction.EXECUTE);
			updateContainerStack.accept(fluidHandler.getContainer());
			return true;
		}
		return false;
	}

	@Override
	public int getMinimumMultiplierRequired() {
		return (int) Math.ceil((float) contents.getAmount() / getBaseCapacity(backpackWrapper));
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}
}