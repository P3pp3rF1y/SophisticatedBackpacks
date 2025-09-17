package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smithing;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.*;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.item.crafting.SmithingRecipeInput;
import net.p3pp3rf1y.sophisticatedcore.common.gui.ICraftingContainer;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;
import net.p3pp3rf1y.sophisticatedcore.util.RecipeHelper;

import java.util.List;

public class SmithingUpgradeContainer extends UpgradeContainerBase<SmithingUpgradeWrapper, SmithingUpgradeContainer> implements ICraftingContainer {
	private static final String DATA_SHIFT_CLICK_INTO_STORAGE = "shiftClickIntoStorage";
	private final Slot resultSlot;
	private Runnable onResultChanged = () -> {};

	private final PersistableSmithingMenu smithingMenuDelegate;
	public SmithingUpgradeContainer(Player player, int upgradeContainerId, SmithingUpgradeWrapper upgradeWrapper, UpgradeContainerType<SmithingUpgradeWrapper, SmithingUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		smithingMenuDelegate = new PersistableSmithingMenu(new Inventory(player));

		slots.add(smithingMenuDelegate.getSlot(SmithingMenu.TEMPLATE_SLOT));
		slots.add(smithingMenuDelegate.getSlot(SmithingMenu.BASE_SLOT));
		slots.add(smithingMenuDelegate.getSlot(SmithingMenu.ADDITIONAL_SLOT));
		resultSlot = smithingMenuDelegate.getSlot(SmithingMenu.RESULT_SLOT);
		slots.add(resultSlot);
		smithingMenuDelegate.createResult();
	}

	public void setOnResultChangedHandler(Runnable onResultChanged) {
		this.onResultChanged = onResultChanged;
	}

	@Override
	public void handlePacket(CompoundTag data) {
		if (data.contains(DATA_SHIFT_CLICK_INTO_STORAGE)) {
			setShiftClickIntoStorage(data.getBoolean(DATA_SHIFT_CLICK_INTO_STORAGE));
		}
	}

	@Override
	public void setUpgradeWrapper(IUpgradeWrapper updatedUpgradeWrapper) {
		super.setUpgradeWrapper(updatedUpgradeWrapper);
		smithingMenuDelegate.createResult();
	}

	public boolean shouldShiftClickIntoStorage() {
		return upgradeWrapper.shouldShiftClickIntoStorage();
	}

	public void setShiftClickIntoStorage(boolean shiftClickIntoStorage) {
		upgradeWrapper.setShiftClickIntoStorage(shiftClickIntoStorage);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundTag(), DATA_SHIFT_CLICK_INTO_STORAGE, shiftClickIntoStorage));
	}

	@Override
	public boolean mergeIntoStorageFirst(Slot slot) {
		return !(slot instanceof ResultSlot) || shouldShiftClickIntoStorage();
	}

	@Override
	public boolean allowsPickupAll(Slot slot) {
		return slot != resultSlot;
	}

	public Slot getTemplateSlot() {
		return smithingMenuDelegate.getSlot(SmithingMenu.TEMPLATE_SLOT);
	}

	public Slot getBaseSlot() {
		return smithingMenuDelegate.getSlot(SmithingMenu.BASE_SLOT);
	}

	public Slot getAdditionalSlot() {
		return smithingMenuDelegate.getSlot(SmithingMenu.ADDITIONAL_SLOT);
	}

	public Slot getResultSlot() {
		return smithingMenuDelegate.getSlot(SmithingMenu.RESULT_SLOT);
	}

	@Override
	public List<Slot> getRecipeSlots() {
		return List.of(getTemplateSlot(), getBaseSlot(), getAdditionalSlot());
	}

	@Override
	public Container getCraftMatrix() {
		return smithingMenuDelegate.getInputSlots();
	}

	@Override
	public void setRecipeUsed(ResourceLocation recipeId) {
		smithingMenuDelegate.setSelectedRecipe(recipeId);
	}

	@Override
	public RecipeType<?> getRecipeType() {
		return RecipeType.SMITHING;
	}

	@Override
	public boolean shouldRefillCraftingGrid() {
		return false;
	}

	private class PersistableSmithingMenu extends SmithingMenu {

		public PersistableSmithingMenu(Inventory playerInventory) {
			super(0, playerInventory, playerInventory.player.level().isClientSide() ? ContainerLevelAccess.NULL : ContainerLevelAccess.create(playerInventory.player.level(), playerInventory.player.blockPosition()));
		}

		@Override
		protected void createInputSlots(ItemCombinerMenuSlotDefinition itemCombinerMenuSlotDefinition) {
			for(final ItemCombinerMenuSlotDefinition.SlotDefinition slotDefinition : itemCombinerMenuSlotDefinition.getSlots()) {
				this.addSlot(new SlotSuppliedHandler(upgradeWrapper::getInventory, slotDefinition.slotIndex(), 0, 0) {
					@Override
					public void setChanged() {
						super.setChanged();
						slotsChanged(inputSlots);
					}

					@Override
					public boolean mayPlace(ItemStack p_267156_) {
						return slotDefinition.mayPlace().test(p_267156_);
					}
				});
			}
		}

		@Override
		protected SimpleContainer createContainer(int size) {
			return new SimpleContainer(size) {
				public void setChanged() {
					super.setChanged();
					slotsChanged(this);
				}

				@Override
				public ItemStack getItem(int pIndex) {
					return upgradeWrapper.getInventory().getStackInSlot(pIndex);
				}

				@Override
				public void setItem(int pIndex, ItemStack pStack) {
					upgradeWrapper.getInventory().setStackInSlot(pIndex, pStack);
				}
			};
		}

		@Override
		protected void createResultSlot(ItemCombinerMenuSlotDefinition slotDefinition) {
			this.addSlot(new Slot(this.resultSlots, slotDefinition.getResultSlot().slotIndex(), slotDefinition.getResultSlot().x(), slotDefinition.getResultSlot().y()) {
				public boolean mayPlace(ItemStack stack) {
					return false;
				}

				public boolean mayPickup(Player player) {
					return PersistableSmithingMenu.this.mayPickup(player, this.hasItem());
				}

				public void onTake(Player player, ItemStack stack) {
					PersistableSmithingMenu.this.onTake(player, stack);
				}

				@Override
				public void setChanged() {
					super.setChanged();
					onResultChanged.run();
				}
			});
		}

		@Override
		public void slotsChanged(Container pInventory) {
			createResult();
			onResultChanged.run();
		}

		public Container getInputSlots() {
			return inputSlots;
		}

		public void setSelectedRecipe(ResourceLocation recipeId) {
			SmithingRecipeInput smithingRecipeInput = new SmithingRecipeInput(getTemplateSlot().getItem(), getBaseSlot().getItem(), getAdditionalSlot().getItem());
			RecipeHelper.safeGetRecipeFor(RecipeType.SMITHING, smithingRecipeInput, recipeId).ifPresent(recipe -> selectedRecipe = recipe);
		}
	}
}
