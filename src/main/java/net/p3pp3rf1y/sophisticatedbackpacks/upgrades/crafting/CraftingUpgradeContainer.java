package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.crafting;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.CraftResultInventory;
import net.minecraft.inventory.CraftingInventory;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.container.CraftingResultSlot;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.ICraftingRecipe;
import net.minecraft.item.crafting.IRecipeType;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.NonNullList;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ICraftingContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Optional;

public class CraftingUpgradeContainer extends UpgradeContainerBase<CraftingUpgradeWrapper, CraftingUpgradeContainer> implements ICraftingContainer {
	private static final String DATA_SHIFT_CLICK_INTO_BACKPACK = "shiftClickIntoBackpack";
	private final CraftResultInventory craftResult = new CraftResultInventory();
	private final CraftingItemHandler craftMatrix;
	private final CraftingResultSlot craftingResultSlot;
	@Nullable
	private ICraftingRecipe lastRecipe = null;

	public CraftingUpgradeContainer(PlayerEntity player, int upgradeContainerId, CraftingUpgradeWrapper upgradeWrapper, UpgradeContainerType<CraftingUpgradeWrapper, CraftingUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);

		int slot;
		for (slot = 0; slot < upgradeWrapper.getInventory().getSlots(); slot++) {
			slots.add(new SlotSuppliedHandler(upgradeWrapper::getInventory, slot, -100, -100) {
				@Override
				public void onSlotChanged() {
					super.onSlotChanged();
					updateCraftingResult(player.world, player, craftMatrix, craftResult, craftingResultSlot);
				}
			});
		}
		craftMatrix = new CraftingItemHandler(upgradeWrapper::getInventory, this::onCraftMatrixChanged);
		craftingResultSlot = new CraftingResultSlot(player, craftMatrix, craftResult, slot, -100, -100) {
			@Override
			public ItemStack onTake(PlayerEntity thePlayer, ItemStack stack) {
				onCrafting(stack);
				net.minecraftforge.common.ForgeHooks.setCraftingPlayer(thePlayer);
				NonNullList<ItemStack> nonnulllist;
				if (lastRecipe != null && lastRecipe.matches(craftMatrix, player.world)) {
					nonnulllist = lastRecipe.getRemainingItems(craftMatrix);
				} else {
					nonnulllist = craftMatrix.stackList;
				}
				net.minecraftforge.common.ForgeHooks.setCraftingPlayer(null);
				for (int i = 0; i < nonnulllist.size(); ++i) {
					ItemStack itemstack = craftMatrix.getStackInSlot(i);
					ItemStack itemstack1 = nonnulllist.get(i);
					if (!itemstack.isEmpty()) {
						craftMatrix.decrStackSize(i, 1);
						itemstack = craftMatrix.getStackInSlot(i);
					}

					if (!itemstack1.isEmpty()) {
						if (itemstack.isEmpty()) {
							craftMatrix.setInventorySlotContents(i, itemstack1);
						} else if (ItemStack.areItemsEqual(itemstack, itemstack1) && ItemStack.areItemStackTagsEqual(itemstack, itemstack1)) {
							itemstack1.grow(itemstack.getCount());
							craftMatrix.setInventorySlotContents(i, itemstack1);
						} else if (!player.inventory.addItemStackToInventory(itemstack1)) {
							player.dropItem(itemstack1, false);
						}
					}
				}

				return stack;
			}
		};
		slots.add(craftingResultSlot);
	}

	@Override
	public void onInit() {
		super.onInit();
		onCraftMatrixChanged(craftMatrix);
	}

	private void onCraftMatrixChanged(IInventory iInventory) {
		updateCraftingResult(player.world, player, craftMatrix, craftResult, craftingResultSlot);
	}

	private void updateCraftingResult(World world, PlayerEntity player, CraftingInventory inventory, CraftResultInventory inventoryResult, CraftingResultSlot craftingResultSlot) {
		if (!world.isRemote) {
			ServerPlayerEntity serverplayerentity = (ServerPlayerEntity) player;
			ItemStack itemstack = ItemStack.EMPTY;
			if (lastRecipe != null && lastRecipe.matches(inventory, world)) {
				itemstack = lastRecipe.getCraftingResult(inventory);
			} else {
				//noinspection ConstantConditions - we're on server and for sure in the world so getServer can't return null here
				Optional<ICraftingRecipe> optional = world.getServer().getRecipeManager().getRecipe(IRecipeType.CRAFTING, inventory, world);
				if (optional.isPresent()) {
					ICraftingRecipe craftingRecipe = optional.get();
					if (inventoryResult.canUseRecipe(world, serverplayerentity, craftingRecipe)) {
						lastRecipe = craftingRecipe;
						itemstack = lastRecipe.getCraftingResult(inventory);
					} else {
						lastRecipe = null;
					}
				}
			}

			craftingResultSlot.putStack(itemstack);
			if (serverplayerentity.openContainer instanceof BackpackContainer) {
				((BackpackContainer) serverplayerentity.openContainer).setSlotStackToUpdate(craftingResultSlot.slotNumber, itemstack);
			}
		}
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SHIFT_CLICK_INTO_BACKPACK)) {
			setShiftClickIntoBackpack(data.getBoolean(DATA_SHIFT_CLICK_INTO_BACKPACK));
		}
	}

	@Override
	public ItemStack getSlotStackToTransfer(Slot slot) {
		if (slot == craftingResultSlot) {
			ItemStack slotStack = slot.getStack();
			slotStack.getItem().onCreated(slotStack, player.world, player);
			return slotStack;
		}
		return super.getSlotStackToTransfer(slot);
	}

	@Override
	public void onTakeFromSlot(Slot slot, PlayerEntity player, ItemStack slotStack) {
		ItemStack remainder = slot.onTake(player, slotStack);
		if (!remainder.isEmpty()) {
			player.dropItem(remainder, false);
		}
	}

	@Override
	public List<Slot> getRecipeSlots() {
		return slots.subList(0, 9);
	}

	@Override
	public IInventory getCraftMatrix() {
		return craftMatrix;
	}

	public boolean shouldShiftClickIntoBackpack() {
		return upgradeWrapper.shouldShiftClickIntoBackpack();
	}

	public void setShiftClickIntoBackpack(boolean shiftClickIntoBackpack) {
		upgradeWrapper.setShiftClickIntoBackpack(shiftClickIntoBackpack);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHIFT_CLICK_INTO_BACKPACK, shiftClickIntoBackpack));
	}

	@Override
	public boolean mergeIntoBackpackFirst(Slot slot) {
		return !(slot instanceof CraftingResultSlot) || shouldShiftClickIntoBackpack();
	}

	@Override
	public boolean allowsPickupAll(Slot slot) {
		return slot != craftingResultSlot;
	}
}
