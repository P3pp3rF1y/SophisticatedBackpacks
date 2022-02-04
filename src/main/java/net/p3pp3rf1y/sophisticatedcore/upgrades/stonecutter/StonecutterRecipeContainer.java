package net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter;

import com.google.common.collect.Lists;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.sounds.SoundSource;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ContainerLevelAccess;
import net.minecraft.world.inventory.DataSlot;
import net.minecraft.world.inventory.ResultContainer;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.item.crafting.StonecutterRecipe;
import net.p3pp3rf1y.sophisticatedcore.common.gui.IServerUpdater;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.CraftingItemHandler;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RecipeHelper;

import java.util.List;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class StonecutterRecipeContainer {
	private static final String DATA_SELECTED_RECIPE_INDEX = "selectedRecipeIndex";
	private final Slot inputSlot;
	private final IServerUpdater serverUpdater;
	private final Slot outputSlot;
	private final ResultContainer resultInventory = new ResultContainer();
	private List<StonecutterRecipe> recipes = Lists.newArrayList();
	private final DataSlot selectedRecipe = DataSlot.standalone();
	private Item inputItem = Items.AIR;
	private final CraftingItemHandler inputInventory;
	private Runnable inventoryUpdateListener = () -> {};
	private final Supplier<Optional<ResourceLocation>> getLastSelectedRecipeId;
	private final Consumer<ResourceLocation> setLastSelectedRecipeId;
	private long lastOnTake = -1;

	public StonecutterRecipeContainer(StonecutterUpgradeContainer upgradeContainer, Consumer<Slot> addSlot, IServerUpdater serverUpdater, ContainerLevelAccess worldPosCallable) {
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

	private void onCraftMatrixChanged(Container inventoryIn) {
		ItemStack itemstack = inputSlot.getItem();
		if (itemstack.getItem() != inputItem) {
			inputItem = itemstack.getItem();
			updateAvailableRecipes(inventoryIn, itemstack);
		}
		inventoryUpdateListener.run();
	}

	private void updateAvailableRecipes(Container inventory, ItemStack stack) {
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

	public List<StonecutterRecipe> getRecipeList() {
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
			serverUpdater.sendDataToServer(() -> NBTHelper.putInt(new CompoundTag(), DATA_SELECTED_RECIPE_INDEX, recipeIndex));
		}
		return true;
	}

	private boolean isIndexInRecipeBounds(int index) {
		return index >= 0 && index < recipes.size();
	}

	private void updateRecipeResultSlot() {
		if (!recipes.isEmpty() && isIndexInRecipeBounds(selectedRecipe.get())) {
			StonecutterRecipe stonecuttingrecipe = recipes.get(selectedRecipe.get());
			resultInventory.setRecipeUsed(stonecuttingrecipe);
			outputSlot.set(stonecuttingrecipe.assemble(inputInventory));
		} else {
			outputSlot.set(ItemStack.EMPTY);
		}
	}

	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_SELECTED_RECIPE_INDEX)) {
			selectRecipe(data.getInt(DATA_SELECTED_RECIPE_INDEX));
		}
	}

	public boolean isNotResultSlot(Slot slot) {
		return slot != outputSlot;
	}

	private class ResultSlot extends Slot {
		private final ContainerLevelAccess worldPosCallable;

		public ResultSlot(ContainerLevelAccess worldPosCallable) {
			super(resultInventory, 1, -1, -1);
			this.worldPosCallable = worldPosCallable;
		}

		@Override
		public boolean mayPlace(ItemStack stack) {
			return false;
		}

		@Override
		public void onTake(Player thePlayer, ItemStack stack) {
			stack.onCraftedBy(thePlayer.level, thePlayer, stack.getCount());
			resultInventory.awardUsedRecipes(thePlayer);
			ItemStack itemstack = inputSlot.remove(1);
			if (!itemstack.isEmpty()) {
				updateRecipeResultSlot();
			}

			worldPosCallable.execute((world, pos) -> {
				long l = world.getGameTime();
				if (lastOnTake != l) {
					world.playSound(null, pos, SoundEvents.UI_STONECUTTER_TAKE_RESULT, SoundSource.BLOCKS, 1.0F, 1.0F);
					lastOnTake = l;
				}
			});
			super.onTake(thePlayer, stack);
		}
	}
}
