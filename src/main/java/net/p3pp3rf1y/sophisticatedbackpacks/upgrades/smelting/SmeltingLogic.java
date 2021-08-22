package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.block.Blocks;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.item.crafting.FurnaceRecipe;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class SmeltingLogic {
	private final ItemStack upgrade;
	private final Consumer<ItemStack> saveHandler;

	private ItemStackHandler smeltingInventory = null;
	public static final int COOK_INPUT_SLOT = 0;
	public static final int COOK_OUTPUT_SLOT = 2;
	public static final int FUEL_SLOT = 1;
	@Nullable
	private FurnaceRecipe smeltingRecipe = null;
	private boolean smeltingRecipeInitialized = false;

	private final Predicate<ItemStack> isFuel;
	private final Predicate<ItemStack> isInput;
	private final double smeltingSpeedMultiplier;
	private final double fuelEfficiencyMultiplier;

	public SmeltingLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, double smeltingSpeedMultiplier, double fuelEfficiencyMultiplier) {
		this(upgrade, saveHandler, s -> getBurnTime(s) > 0, s -> RecipeHelper.getSmeltingRecipe(s).isPresent(), smeltingSpeedMultiplier, fuelEfficiencyMultiplier);
	}

	public SmeltingLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, Predicate<ItemStack> isFuel, Predicate<ItemStack> isInput, double smeltingSpeedMultiplier, double fuelEfficiencyMultiplier) {
		this.upgrade = upgrade;
		this.saveHandler = saveHandler;
		this.isFuel = isFuel;
		this.isInput = isInput;
		this.smeltingSpeedMultiplier = smeltingSpeedMultiplier;
		this.fuelEfficiencyMultiplier = fuelEfficiencyMultiplier;
	}

	private void save() {
		saveHandler.accept(upgrade);
	}

	public boolean tick(World world) {
		AtomicBoolean didSomething = new AtomicBoolean(true);
		if (isBurning(world) || readyToStartCooking()) {
			Optional<FurnaceRecipe> fr = getSmeltingRecipe();
			if (!fr.isPresent() && isCooking()) {
				setIsCooking(false);
			}
			fr.ifPresent(recipe -> {
				updateFuel(world, recipe);

				if (isBurning(world) && canSmelt(recipe)) {
					updateCookingProgress(world, recipe);
				} else if (!isBurning(world)) {
					didSomething.set(false);
				}
			});
		}

		if (!isBurning(world) && isCooking()) {
			updateCookingCooldown(world);
		} else {
			didSomething.set(false);
		}
		return didSomething.get();
	}

	public boolean isBurning(World world) {
		return getBurnTimeFinish() >= world.getGameTime();
	}

	private Optional<FurnaceRecipe> getSmeltingRecipe() {
		if (!smeltingRecipeInitialized) {
			smeltingRecipe = RecipeHelper.getSmeltingRecipe(getCookInput()).orElse(null);
			smeltingRecipeInitialized = true;
		}
		return Optional.ofNullable(smeltingRecipe);
	}

	private void updateCookingCooldown(World world) {
		if (getRemainingCookTime(world) + 2 > getCookTimeTotal()) {
			setIsCooking(false);
		} else {
			setCookTimeFinish(world.getGameTime() + Math.min(getRemainingCookTime(world) + 2, getCookTimeTotal()));
		}
	}

	private void updateCookingProgress(World world, FurnaceRecipe smeltingRecipe) {
		if (isCooking() && finishedCooking(world)) {
			smelt(smeltingRecipe);
			if (canSmelt(smeltingRecipe)) {
				setCookTime(world, (int) (smeltingRecipe.getCookingTime() * (1 / smeltingSpeedMultiplier)));
			} else {
				setIsCooking(false);
			}
		} else if (!isCooking()) {
			setIsCooking(true);
			setCookTime(world, (int) (smeltingRecipe.getCookingTime() * (1 / smeltingSpeedMultiplier)));
		}
	}

	private boolean finishedCooking(World world) {
		return getCookTimeFinish() <= world.getGameTime();
	}

	private boolean readyToStartCooking() {
		return !getFuel().isEmpty() && !getCookInput().isEmpty();
	}

	private void smelt(IRecipe<?> recipe) {
		if (!canSmelt(recipe)) {
			return;
		}

		ItemStack input = getCookInput();
		ItemStack recipeOutput = recipe.getResultItem();
		ItemStack output = getCookOutput();
		if (output.isEmpty()) {
			setCookOutput(recipeOutput.copy());
		} else if (output.getItem() == recipeOutput.getItem()) {
			output.grow(recipeOutput.getCount());
			setCookOutput(output);
		}

		if (input.getItem() == Blocks.WET_SPONGE.asItem() && !getFuel().isEmpty() && getFuel().getItem() == Items.BUCKET) {
			setFuel(new ItemStack(Items.WATER_BUCKET));
		}

		input.shrink(1);
		setCookInput(input);
	}

	public void setCookInput(ItemStack input) {
		smeltingInventory.setStackInSlot(COOK_INPUT_SLOT, input);
	}

	private void setCookOutput(ItemStack stack) {
		getSmeltingInventory().setStackInSlot(COOK_OUTPUT_SLOT, stack);
	}

	private int getRemainingCookTime(World world) {
		return (int) (getCookTimeFinish() - world.getGameTime());
	}

	private void setCookTime(World world, int cookTime) {
		setCookTimeFinish(world.getGameTime() + cookTime);
		setCookTimeTotal(cookTime);
	}

	private void updateFuel(World world, FurnaceRecipe smeltingRecipe) {
		ItemStack fuel = getFuel();
		if (!isBurning(world) && canSmelt(smeltingRecipe)) {
			if (getBurnTime(fuel) <= 0) {
				return;
			}
			setBurnTime(world, (int) (getBurnTime(fuel) * fuelEfficiencyMultiplier / smeltingSpeedMultiplier));
			if (isBurning(world)) {
				if (fuel.hasContainerItem()) {
					setFuel(fuel.getContainerItem());
				} else if (!fuel.isEmpty()) {
					fuel.shrink(1);
					setFuel(fuel);
					if (fuel.isEmpty()) {
						setFuel(fuel.getContainerItem());
					}
				}
			}
		}
	}

	private void setBurnTime(World world, int burnTime) {
		setBurnTimeFinish(world.getGameTime() + burnTime);
		setBurnTimeTotal(burnTime);
	}

	protected boolean canSmelt(IRecipe<?> smeltingRecipe) {
		if (getCookInput().isEmpty()) {
			return false;
		}
		ItemStack recipeOutput = smeltingRecipe.getResultItem();
		if (recipeOutput.isEmpty()) {
			return false;
		} else {
			ItemStack output = getCookOutput();
			if (output.isEmpty()) {
				return true;
			} else if (!output.sameItem(recipeOutput)) {
				return false;
			} else if (output.getCount() + recipeOutput.getCount() <= 64 && output.getCount() + recipeOutput.getCount() <= output.getMaxStackSize()) {
				return true;
			} else {
				return output.getCount() + recipeOutput.getCount() <= recipeOutput.getMaxStackSize();
			}
		}
	}

	private static int getBurnTime(ItemStack fuel) {
		return ForgeHooks.getBurnTime(fuel);
	}

	public ItemStack getCookOutput() {
		return getSmeltingInventory().getStackInSlot(COOK_OUTPUT_SLOT);
	}

	public ItemStack getCookInput() {
		return getSmeltingInventory().getStackInSlot(COOK_INPUT_SLOT);
	}

	public ItemStack getFuel() {
		return getSmeltingInventory().getStackInSlot(FUEL_SLOT);
	}

	public void setFuel(ItemStack fuel) {
		getSmeltingInventory().setStackInSlot(FUEL_SLOT, fuel);
	}

	public ItemStackHandler getSmeltingInventory() {
		if (smeltingInventory == null) {
			smeltingInventory = new ItemStackHandler(3) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					upgrade.addTagElement("smeltingInventory", serializeNBT());
					save();
					if (slot == COOK_INPUT_SLOT) {
						smeltingRecipeInitialized = false;
					}
				}

				@Override
				public boolean isItemValid(int slot, ItemStack stack) {
					switch (slot) {
						case COOK_INPUT_SLOT:
							return isInput.test(stack);
						case FUEL_SLOT:
							return isFuel.test(stack);
						default:
							return true;
					}
				}
			};
			NBTHelper.getCompound(upgrade, "smeltingInventory").ifPresent(smeltingInventory::deserializeNBT);
		}
		return smeltingInventory;
	}

	public long getBurnTimeFinish() {
		return NBTHelper.getLong(upgrade, "burnTimeFinish").orElse(0L);
	}

	private void setBurnTimeFinish(long burnTimeFinish) {
		NBTHelper.setLong(upgrade, "burnTimeFinish", burnTimeFinish);
		save();
	}

	public int getBurnTimeTotal() {
		return NBTHelper.getInt(upgrade, "burnTimeTotal").orElse(0);
	}

	private void setBurnTimeTotal(int burnTimeTotal) {
		NBTHelper.setInteger(upgrade, "burnTimeTotal", burnTimeTotal);
		save();
	}

	public long getCookTimeFinish() {
		return NBTHelper.getLong(upgrade, "cookTimeFinish").orElse(-1L);
	}

	private void setCookTimeFinish(long cookTimeFinish) {
		NBTHelper.setLong(upgrade, "cookTimeFinish", cookTimeFinish);
		save();
	}

	public int getCookTimeTotal() {
		return NBTHelper.getInt(upgrade, "cookTimeTotal").orElse(0);
	}

	private void setCookTimeTotal(int cookTimeTotal) {
		NBTHelper.setInteger(upgrade, "cookTimeTotal", cookTimeTotal);
		save();
	}

	public boolean isCooking() {
		return NBTHelper.getBoolean(upgrade, "isCooking").orElse(false);
	}

	private void setIsCooking(boolean isCooking) {
		NBTHelper.setBoolean(upgrade, "isCooking", isCooking);
		save();
	}
}
