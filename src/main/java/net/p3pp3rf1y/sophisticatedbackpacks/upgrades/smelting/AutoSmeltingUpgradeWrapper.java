package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.entity.LivingEntity;
import net.minecraft.item.ItemStack;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class AutoSmeltingUpgradeWrapper extends UpgradeWrapperBase<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeItem>
		implements ITickableUpgrade, ISmeltingUpgrade {
	private static final int NOTHING_TO_DO_COOLDOWN = 10;
	private static final int NO_INVENTORY_SPACE_COOLDOWN = 60;

	private final FilterLogic inputFilterLogic;
	private final FilterLogic fuelFilterLogic;
	private final SmeltingLogic smeltingLogic;
	private final Predicate<ItemStack> isValidInput;
	private final Predicate<ItemStack> isValidFuel;
	private int outputCooldown = 0;
	private int fuelCooldown = 0;
	private int inputCooldown = 0;

	public AutoSmeltingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		inputFilterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.autoSmeltingUpgrade.inputFilterSlots.get(),
				s -> RecipeHelper.getSmeltingRecipe(s).isPresent(), "inputFilter");
		fuelFilterLogic = new FilterLogic(upgrade, upgradeSaveHandler, Config.COMMON.autoSmeltingUpgrade.fuelFilterSlots.get(),
				s -> ForgeHooks.getBurnTime(s) > 0, "fuelFilter");
		fuelFilterLogic.setAllowByDefault();
		fuelFilterLogic.setEmptyAllowListMatchesEverything();

		isValidInput = s -> RecipeHelper.getSmeltingRecipe(s).isPresent() && inputFilterLogic.matchesFilter(s);
		isValidFuel = s -> ForgeHooks.getBurnTime(s) > 0 && fuelFilterLogic.matchesFilter(s);
		smeltingLogic = new SmeltingLogic(upgrade, upgradeSaveHandler, isValidFuel, isValidInput, Config.COMMON.autoSmeltingUpgrade.smeltingSpeedMultiplier.get(),
				Config.COMMON.autoSmeltingUpgrade.fuelEfficiencyMultiplier.get());
	}

	private void tryPushingOutput() {
		if (outputCooldown > 0) {
			outputCooldown--;
			return;
		}

		ItemStack output = smeltingLogic.getCookOutput();
		IItemHandlerModifiable inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		if (!output.isEmpty() && InventoryHelper.insertIntoInventory(output, inventory, true).getCount() < output.getCount()) {
			ItemStack ret = InventoryHelper.insertIntoInventory(output, inventory, false);
			smeltingLogic.getSmeltingInventory().extractItem(SmeltingLogic.COOK_OUTPUT_SLOT, output.getCount() - ret.getCount(), false);
		} else {
			outputCooldown = NO_INVENTORY_SPACE_COOLDOWN;
		}

		ItemStack fuel = smeltingLogic.getFuel();
		if (!fuel.isEmpty() && ForgeHooks.getBurnTime(fuel) <= 0 && InventoryHelper.insertIntoInventory(fuel, inventory, true).getCount() < fuel.getCount()) {
			ItemStack ret = InventoryHelper.insertIntoInventory(fuel, inventory, false);
			smeltingLogic.getSmeltingInventory().extractItem(SmeltingLogic.FUEL_SLOT, fuel.getCount() - ret.getCount(), false);
		}
	}

	@Override
	public void tick(@Nullable LivingEntity entity, World world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}
		tryPushingOutput();
		tryPullingFuel();
		tryPullingInput();

		if (!smeltingLogic.tick(world) && outputCooldown <= 0 && fuelCooldown <= 0 && inputCooldown <= 0) {
			setCooldown(world, NOTHING_TO_DO_COOLDOWN);
		}
	}

	private void tryPullingInput() {
		if (inputCooldown > 0) {
			inputCooldown--;
			return;
		}

		if (tryPullingGetUnsucessful(smeltingLogic.getCookInput(), smeltingLogic::setCookInput, isValidInput)) {
			inputCooldown = NO_INVENTORY_SPACE_COOLDOWN;
		}
	}

	private void tryPullingFuel() {
		if (fuelCooldown > 0) {
			fuelCooldown--;
			return;
		}

		if (tryPullingGetUnsucessful(smeltingLogic.getFuel(), smeltingLogic::setFuel, isValidFuel)) {
			fuelCooldown = NO_INVENTORY_SPACE_COOLDOWN;
		}
	}

	private boolean tryPullingGetUnsucessful(ItemStack stack, Consumer<ItemStack> setSlot, Predicate<ItemStack> isItemValid) {
		ItemStack toExtract;
		IItemHandlerModifiable inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		if (stack.isEmpty()) {
			AtomicReference<ItemStack> ret = new AtomicReference<>(ItemStack.EMPTY);
			InventoryHelper.iterate(inventory, (slot, st) -> {
				if (isItemValid.test(st)) {
					ret.set(st.copy());
				}
			}, () -> !ret.get().isEmpty());
			if (!ret.get().isEmpty()) {
				toExtract = ret.get();
				toExtract.setCount(toExtract.getMaxStackSize());
			} else {
				return true;
			}
		} else if (stack.getCount() == stack.getMaxStackSize() || !isItemValid.test(stack)) {
			return true;
		} else {
			toExtract = stack.copy();
			toExtract.setCount(stack.getMaxStackSize() - stack.getCount());
		}

		if (InventoryHelper.extractFromInventory(toExtract, inventory, true).getCount() > 0) {
			ItemStack toSet = InventoryHelper.extractFromInventory(toExtract, inventory, false);
			toSet.grow(stack.getCount());
			setSlot.accept(toSet);
		} else {
			return true;
		}
		return false;
	}

	@Override
	public SmeltingLogic getSmeltingLogic() {
		return smeltingLogic;
	}

	public FilterLogic getInputFilterLogic() {
		return inputFilterLogic;
	}

	public FilterLogic getFuelFilterLogic() {
		return fuelFilterLogic;
	}
}
