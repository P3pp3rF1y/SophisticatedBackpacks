package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.furnace;

import net.minecraft.block.Blocks;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.item.crafting.FurnaceRecipe;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.item.crafting.IRecipeType;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ForgeHooks;
import net.minecraftforge.items.ItemStackHandler;
import net.minecraftforge.items.wrapper.RecipeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.Optional;
import java.util.function.Consumer;

public class FurnaceUpgradeWrapper extends UpgradeWrapperBase<FurnaceUpgradeWrapper, FurnaceUpgradeItem> implements ITickableUpgrade {
	private ItemStackHandler furnaceInventory = null;
	public static final int COOK_INPUT_SLOT = 0;
	public static final int COOK_OUTPUT_SLOT = 2;
	public static final int FUEL_SLOT = 1;
	private RecipeWrapper recipeWrapper = null;

	public FurnaceUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(upgrade, upgradeSaveHandler);
	}

	public boolean isBurning(World world) {
		return getBurnTimeFinish() > world.getGameTime();
	}

	@Override
	public void tick(@Nullable PlayerEntity player, World world, BlockPos pos, IBackpackWrapper wrapper) {
		if (world.isRemote) {
			return;
		}
		if (isBurning(world) || readyToStartCooking()) {
			FurnaceRecipe furnaceRecipe = getSmeltingRecipe(world).orElse(null);
			if (furnaceRecipe == null) {
				if (isCooking()) {
					setIsCooking(false);
				}
				return;
			}
			updateFuel(world, furnaceRecipe);

			if (isBurning(world) && canSmelt(furnaceRecipe)) {
				updateCookingProgress(world, furnaceRecipe);
			}
		} else if (!isBurning(world) && isCooking()) {
			updateCookingCooldown(world);
		}
	}

	private void updateCookingCooldown(World world) {
		if (getRemainingCookTime(world) + 2 > getCookTimeTotal()) {
			setIsCooking(false);
		} else {
			setCookTimeFinish(world.getGameTime() + Math.min(getRemainingCookTime(world) + 2, getCookTimeTotal()));
		}
	}

	private void updateCookingProgress(World world, FurnaceRecipe furnaceRecipe) {
		if (isCooking() && finishedCooking(world)) {
			setCookTime(world, furnaceRecipe.getCookTime());
			smelt(furnaceRecipe);
		} else if (!isCooking()) {
			setIsCooking(true);
			setCookTime(world, furnaceRecipe.getCookTime());
		}
	}

	private boolean finishedCooking(World world) {
		return getCookTimeFinish() <= world.getGameTime();
	}

	private boolean readyToStartCooking() {
		return !getFuel().isEmpty() && !getCookInput().isEmpty();
	}

	private Optional<FurnaceRecipe> getSmeltingRecipe(World world) {
		return world.getRecipeManager().getRecipe(IRecipeType.SMELTING, getRecipeWrapper(), world);
	}

	private void smelt(IRecipe<?> recipe) {
		if (!canSmelt(recipe)) {
			return;
		}

		ItemStack input = getCookInput();
		ItemStack recipeOutput = recipe.getRecipeOutput();
		ItemStack output = getCookOutput();
		if (output.isEmpty()) {
			setCookOutput(recipeOutput.copy());
		} else if (output.getItem() == recipeOutput.getItem()) {
			output.grow(recipeOutput.getCount());
		}

		if (input.getItem() == Blocks.WET_SPONGE.asItem() && !getFuel().isEmpty() && getFuel().getItem() == Items.BUCKET) {
			setFuel(new ItemStack(Items.WATER_BUCKET));
		}

		input.shrink(1);
		setCookInput(input);
	}

	private void setCookInput(ItemStack input) {
		furnaceInventory.setStackInSlot(COOK_INPUT_SLOT, input);
	}

	private void setCookOutput(ItemStack stack) {
		getFurnaceInventory().setStackInSlot(COOK_OUTPUT_SLOT, stack);
	}

	private int getRemainingCookTime(World world) {
		return (int) (getCookTimeFinish() - world.getGameTime());
	}

	private void setCookTime(World world, int cookTime) {
		setCookTimeFinish(world.getGameTime() + cookTime);
		setCookTimeTotal(cookTime);
	}

	private void updateFuel(World world, FurnaceRecipe furnaceRecipe) {
		ItemStack fuel = getFuel();
		if (!isBurning(world) && canSmelt(furnaceRecipe)) {
			setBurnTime(world, getBurnTime(fuel));
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

	protected boolean canSmelt(IRecipe<?> furnaceRecipe) {
		if (getCookInput().isEmpty()) {
			return false;
		}
		ItemStack recipeOutput = furnaceRecipe.getRecipeOutput();
		if (recipeOutput.isEmpty()) {
			return false;
		} else {
			ItemStack output = getCookOutput();
			if (output.isEmpty()) {
				return true;
			} else if (!output.isItemEqual(recipeOutput)) {
				return false;
			} else if (output.getCount() + recipeOutput.getCount() <= 64 && output.getCount() + recipeOutput.getCount() <= output.getMaxStackSize()) {
				return true;
			} else {
				return output.getCount() + recipeOutput.getCount() <= recipeOutput.getMaxStackSize();
			}
		}
	}

	protected int getBurnTime(ItemStack fuel) {
		if (fuel.isEmpty()) {
			return 0;
		} else {
			return ForgeHooks.getBurnTime(fuel);
		}
	}

	private ItemStack getCookOutput() {
		return getFurnaceInventory().getStackInSlot(COOK_OUTPUT_SLOT);
	}

	private ItemStack getCookInput() {
		return getFurnaceInventory().getStackInSlot(COOK_INPUT_SLOT);
	}

	private ItemStack getFuel() {
		return getFurnaceInventory().getStackInSlot(FUEL_SLOT);
	}

	private void setFuel(ItemStack fuel) {
		getFurnaceInventory().setStackInSlot(FUEL_SLOT, fuel);
	}

	public ItemStackHandler getFurnaceInventory() {
		if (furnaceInventory == null) {
			furnaceInventory = new ItemStackHandler(3) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					upgrade.setTagInfo("furnaceInventory", serializeNBT());
					save();
				}

				@Override
				public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
					switch (slot) {
						case COOK_INPUT_SLOT:
							return RecipeHelper.getSmeltingRecipe(stack).isPresent();
						case FUEL_SLOT:
							return getBurnTime(stack) > 0;
						default:
							return true;
					}
				}
			};
			NBTHelper.getCompound(upgrade, "furnaceInventory").ifPresent(furnaceInventory::deserializeNBT);
		}
		return furnaceInventory;
	}

	public IInventory getRecipeWrapper() {
		if (recipeWrapper == null) {
			recipeWrapper = new RecipeWrapper(getFurnaceInventory());
		}
		return recipeWrapper;
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
