package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import com.google.common.collect.Lists;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.CraftResultInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.Items;
import net.minecraft.item.crafting.StonecuttingRecipe;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.IWorldPosCallable;
import net.minecraft.util.IntReferenceHolder;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.SoundCategory;
import net.minecraft.util.SoundEvents;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting.CraftingItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RecipeHelper;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class StonecutterRecipeContainer {
	private static final String DATA_SELECTED_RECIPE_INDEX = "selectedRecipeIndex";
	private final Slot inputSlot;
	private final IServerUpdater serverUpdater;
	private final Slot outputSlot;
	private final CraftResultInventory resultInventory = new CraftResultInventory();
	private List<StonecuttingRecipe> recipes = Lists.newArrayList();
	private final IntReferenceHolder selectedRecipe = IntReferenceHolder.standalone();
	private Item inputItem = Items.AIR;
	private final CraftingItemHandler inputInventory;
	private Runnable inventoryUpdateListener = () -> {};
	private final Supplier<Optional<ResourceLocation>> getLastSelectedRecipeId;
	private final Consumer<ResourceLocation> setLastSelectedRecipeId;
	private long lastOnTake = -1;

	public StonecutterRecipeContainer(StonecutterUpgradeContainer upgradeContainer, Consumer<Slot> addSlot, IServerUpdater serverUpdater, IWorldPosCallable worldPosCallable) {
		inputSlot = new SlotSuppliedHandler(upgradeContainer.getUpgradeWrapper()::getInputInventory, 0, -1, -1) {
			@Override
			public void setChanged() {
				super.setChanged();
				onCraftMatrixChanged(inputInventory);
			}

			@Override
			public ItemStack remove(int amount) {
				ItemStack ret = super.remove(amount);
				if (getItem().isEmpty()) {
					setChanged();
				}
				return ret;
			}
		};
		this.serverUpdater = serverUpdater;
		addSlot.accept(inputSlot);
		inputInventory = new CraftingItemHandler(upgradeContainer.getUpgradeWrapper()::getInputInventory, this::onCraftMatrixChanged);
		outputSlot = new ResultSlot(worldPosCallable);
		addSlot.accept(outputSlot);

		getLastSelectedRecipeId = upgradeContainer.getUpgradeWrapper()::getRecipeId;
		setLastSelectedRecipeId = upgradeContainer.getUpgradeWrapper()::setRecipeId;

		onCraftMatrixChanged(inputInventory);
	}

	private void onCraftMatrixChanged(IInventory inventoryIn) {
		ItemStack itemstack = inputSlot.getItem();
		if (itemstack.getItem() != inputItem) {
			inputItem = itemstack.getItem();
			updateAvailableRecipes(inventoryIn, itemstack);
		}
		inventoryUpdateListener.run();
	}

	private void updateAvailableRecipes(IInventory inventory, ItemStack stack) {
		recipes.clear();
		selectedRecipe.set(-1);
		outputSlot.set(ItemStack.EMPTY);
		if (!stack.isEmpty()) {
			recipes = RecipeHelper.getStonecuttingRecipes(inventory);
			getLastSelectedRecipeId.get().ifPresent(id -> {
				for (int i = 0; i < recipes.size(); i++) {
					if (recipes.get(i).getId().equals(id)) {
						selectedRecipe.set(i);
						updateRecipeResultSlot();
					}
				}
			});
		}
	}

	public Slot getInputSlot() {
		return inputSlot;
	}

	public Slot getOutputSlot() {
		return outputSlot;
	}

	public void setInventoryUpdateListener(Runnable listenerIn) {
		inventoryUpdateListener = listenerIn;
	}

	public List<StonecuttingRecipe> getRecipeList() {
		return recipes;
	}

	public int getSelectedRecipe() {
		return selectedRecipe.get();
	}

	public boolean hasItemsInInputSlot() {
		return inputSlot.hasItem() && !recipes.isEmpty();
	}

	public boolean selectRecipe(int recipeIndex) {
		if (isIndexInRecipeBounds(recipeIndex)) {
			selectedRecipe.set(recipeIndex);
			setLastSelectedRecipeId.accept(recipes.get(recipeIndex).getId());
			updateRecipeResultSlot();
			serverUpdater.sendDataToServer(() -> NBTHelper.putInt(new CompoundNBT(), DATA_SELECTED_RECIPE_INDEX, recipeIndex));
		}
		return true;
	}

	private boolean isIndexInRecipeBounds(int index) {
		return index >= 0 && index < recipes.size();
	}

	private void updateRecipeResultSlot() {
		if (!recipes.isEmpty() && isIndexInRecipeBounds(selectedRecipe.get())) {
			StonecuttingRecipe stonecuttingrecipe = recipes.get(selectedRecipe.get());
			resultInventory.setRecipeUsed(stonecuttingrecipe);
			outputSlot.set(stonecuttingrecipe.assemble(inputInventory));
		} else {
			outputSlot.set(ItemStack.EMPTY);
		}
	}

	public boolean handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SELECTED_RECIPE_INDEX)) {
			selectRecipe(data.getInt(DATA_SELECTED_RECIPE_INDEX));
			return true;
		}
		return false;
	}

	public boolean isNotResultSlot(Slot slot) {
		return slot != outputSlot;
	}

	private class ResultSlot extends Slot {
		private final IWorldPosCallable worldPosCallable;

		public ResultSlot(IWorldPosCallable worldPosCallable) {
			super(resultInventory, 1, -1, -1);
			this.worldPosCallable = worldPosCallable;
		}

		@Override
		public boolean mayPlace(ItemStack stack) {
			return false;
		}

		@Override
		public ItemStack onTake(PlayerEntity thePlayer, ItemStack stack) {
			stack.onCraftedBy(thePlayer.level, thePlayer, stack.getCount());
			resultInventory.awardUsedRecipes(thePlayer);
			ItemStack itemstack = inputSlot.remove(1);
			if (!itemstack.isEmpty()) {
				updateRecipeResultSlot();
			}

			worldPosCallable.execute((world, pos) -> {
				long l = world.getGameTime();
				if (lastOnTake != l) {
					world.playSound(null, pos, SoundEvents.UI_STONECUTTER_TAKE_RESULT, SoundCategory.BLOCKS, 1.0F, 1.0F);
					lastOnTake = l;
				}
			});
			return super.onTake(thePlayer, stack);
		}
	}
}
