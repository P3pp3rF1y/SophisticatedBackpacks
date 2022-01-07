package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.item.crafting.BlastingRecipe;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.item.crafting.SmeltingRecipe;
import net.minecraft.world.item.crafting.SmokingRecipe;
import net.minecraft.world.level.Level;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackRenderInfo;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IItemHandlerSimpleInserter;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import javax.annotation.Nullable;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class AutoCookingUpgradeWrapper<W extends AutoCookingUpgradeWrapper<W, U, R>, U extends UpgradeItemBase<W>, R extends AbstractCookingRecipe>
		extends UpgradeWrapperBase<W, U>
		implements ITickableUpgrade, ICookingUpgrade<R> {
	private static final int NOTHING_TO_DO_COOLDOWN = 10;
	private static final int NO_INVENTORY_SPACE_COOLDOWN = 60;

	private final FilterLogic inputFilterLogic;
	private final FilterLogic fuelFilterLogic;
	private final CookingLogic<R> cookingLogic;
	private final Predicate<ItemStack> isValidInput;
	private final Predicate<ItemStack> isValidFuel;
	private final RecipeType<R> recipeType;
	private int outputCooldown = 0;
	private int fuelCooldown = 0;
	private int inputCooldown = 0;

	public AutoCookingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, Config.Common.AutoCookingUpgradeConfig autoCookingConfig, RecipeType<R> recipeType, float burnTimeModifier) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);
		this.recipeType = recipeType;
		inputFilterLogic = new FilterLogic(upgrade, upgradeSaveHandler, autoCookingConfig.inputFilterSlots.get(),
				s -> RecipeHelper.getCookingRecipe(s, recipeType).isPresent(), "inputFilter");
		fuelFilterLogic = new FilterLogic(upgrade, upgradeSaveHandler, autoCookingConfig.fuelFilterSlots.get(),
				s -> ForgeHooks.getBurnTime(s, recipeType) > 0, "fuelFilter");
		fuelFilterLogic.setAllowByDefault();
		fuelFilterLogic.setEmptyAllowListMatchesEverything();

		isValidInput = s -> RecipeHelper.getCookingRecipe(s, recipeType).isPresent() && inputFilterLogic.matchesFilter(s);
		isValidFuel = s -> ForgeHooks.getBurnTime(s, recipeType) > 0 && fuelFilterLogic.matchesFilter(s);
		cookingLogic = new CookingLogic<>(upgrade, upgradeSaveHandler, isValidFuel, isValidInput, autoCookingConfig, recipeType, burnTimeModifier);
	}

	@Override
	public void setEnabled(boolean enabled) {
		if (!enabled) {
			pauseAndRemoveRenderInfo();
		}
		super.setEnabled(enabled);
	}

	private void pauseAndRemoveRenderInfo() {
		cookingLogic.pause();
		BackpackRenderInfo renderInfo = backpackWrapper.getRenderInfo();
		renderInfo.removeUpgradeRenderData(CookingUpgradeRenderData.TYPE);
	}

	@Override
	public void onBeforeRemoved() {
		pauseAndRemoveRenderInfo();
	}

	private void tryPushingOutput() {
		if (outputCooldown > 0) {
			outputCooldown--;
			return;
		}

		ItemStack output = cookingLogic.getCookOutput();
		IItemHandlerSimpleInserter inventory = backpackWrapper.getInventoryForUpgradeProcessing();
		if (!output.isEmpty() && inventory.insertItem(output, true).getCount() < output.getCount()) {
			ItemStack ret = inventory.insertItem(output, false);
			cookingLogic.getCookingInventory().extractItem(CookingLogic.COOK_OUTPUT_SLOT, output.getCount() - ret.getCount(), false);
		} else {
			outputCooldown = NO_INVENTORY_SPACE_COOLDOWN;
		}

		ItemStack fuel = cookingLogic.getFuel();
		if (!fuel.isEmpty() && ForgeHooks.getBurnTime(fuel, recipeType) <= 0 && inventory.insertItem(fuel, true).getCount() < fuel.getCount()) {
			ItemStack ret = inventory.insertItem(fuel, false);
			cookingLogic.getCookingInventory().extractItem(CookingLogic.FUEL_SLOT, fuel.getCount() - ret.getCount(), false);
		}
	}

	@Override
	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}
		tryPushingOutput();
		tryPullingFuel();
		tryPullingInput();

		if (!cookingLogic.tick(world) && outputCooldown <= 0 && fuelCooldown <= 0 && inputCooldown <= 0) {
			setCooldown(world, NOTHING_TO_DO_COOLDOWN);
		}
		boolean isBurning = cookingLogic.isBurning(world);
		BackpackRenderInfo renderInfo = backpackWrapper.getRenderInfo();
		if (renderInfo.getUpgradeRenderData(CookingUpgradeRenderData.TYPE).map(CookingUpgradeRenderData::isBurning).orElse(false) != isBurning) {
			if (isBurning) {
				renderInfo.setUpgradeRenderData(CookingUpgradeRenderData.TYPE, new CookingUpgradeRenderData(true));
			} else {
				renderInfo.removeUpgradeRenderData(CookingUpgradeRenderData.TYPE);
			}
		}
	}

	private void tryPullingInput() {
		if (inputCooldown > 0) {
			inputCooldown--;
			return;
		}

		if (tryPullingGetUnsucessful(cookingLogic.getCookInput(), cookingLogic::setCookInput, isValidInput)) {
			inputCooldown = NO_INVENTORY_SPACE_COOLDOWN;
		}
	}

	private void tryPullingFuel() {
		if (fuelCooldown > 0) {
			fuelCooldown--;
			return;
		}

		if (tryPullingGetUnsucessful(cookingLogic.getFuel(), cookingLogic::setFuel, isValidFuel)) {
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
	public CookingLogic<R> getCookingLogic() {
		return cookingLogic;
	}

	public FilterLogic getInputFilterLogic() {
		return inputFilterLogic;
	}

	public FilterLogic getFuelFilterLogic() {
		return fuelFilterLogic;
	}

	public static class AutoSmeltingUpgradeWrapper extends AutoCookingUpgradeWrapper<AutoSmeltingUpgradeWrapper, AutoSmeltingUpgradeItem, SmeltingRecipe> {
		public AutoSmeltingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler, Config.COMMON.autoSmeltingUpgrade, RecipeType.SMELTING, 1);
		}
	}

	public static class AutoSmokingUpgradeWrapper extends AutoCookingUpgradeWrapper<AutoSmokingUpgradeWrapper, AutoSmokingUpgradeItem, SmokingRecipe> {
		public AutoSmokingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler, Config.COMMON.autoSmokingUpgrade, RecipeType.SMOKING, 0.5f);
		}
	}

	public static class AutoBlastingUpgradeWrapper extends AutoCookingUpgradeWrapper<AutoBlastingUpgradeWrapper, AutoBlastingUpgradeItem, BlastingRecipe> {
		public AutoBlastingUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler, Config.COMMON.autoBlastingUpgrade, RecipeType.BLASTING, 0.5f);
		}
	}
}
