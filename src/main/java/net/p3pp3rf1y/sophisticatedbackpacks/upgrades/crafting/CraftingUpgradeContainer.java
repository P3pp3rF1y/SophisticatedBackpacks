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
				public void setChanged() {
					super.setChanged();
					updateCraftingResult(player.level, player, craftMatrix, craftResult, craftingResultSlot);
				}
			});
		}
		craftMatrix = new CraftingItemHandler(upgradeWrapper::getInventory, this::onCraftMatrixChanged);
		craftingResultSlot = new CraftingResultSlot(player, craftMatrix, craftResult, slot, -100, -100) {
			@Override
			public ItemStack onTake(PlayerEntity thePlayer, ItemStack stack) {
				checkTakeAchievements(stack);
				net.minecraftforge.common.ForgeHooks.setCraftingPlayer(thePlayer);
				NonNullList<ItemStack> nonnulllist;
				if (lastRecipe != null && lastRecipe.matches(craftMatrix, player.level)) {
					nonnulllist = lastRecipe.getRemainingItems(craftMatrix);
				} else {
					nonnulllist = craftMatrix.items;
				}
				net.minecraftforge.common.ForgeHooks.setCraftingPlayer(null);
				for (int i = 0; i < nonnulllist.size(); ++i) {
					ItemStack itemstack = craftMatrix.getItem(i);
					ItemStack itemstack1 = nonnulllist.get(i);
					if (!itemstack.isEmpty()) {
						craftMatrix.removeItem(i, 1);
						itemstack = craftMatrix.getItem(i);
					}

					if (!itemstack1.isEmpty()) {
						if (itemstack.isEmpty()) {
							craftMatrix.setItem(i, itemstack1);
						} else if (ItemStack.isSame(itemstack, itemstack1) && ItemStack.tagMatches(itemstack, itemstack1)) {
							itemstack1.grow(itemstack.getCount());
							craftMatrix.setItem(i, itemstack1);
						} else if (!player.inventory.add(itemstack1)) {
							player.drop(itemstack1, false);
						}
					}
					if (thePlayer.containerMenu instanceof BackpackContainer) {
						Slot slot = slots.get(i);
						((BackpackContainer) thePlayer.containerMenu).setSlotStackToUpdate(slot.index, slot.getItem());
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
		updateCraftingResult(player.level, player, craftMatrix, craftResult, craftingResultSlot);
	}

	private void updateCraftingResult(World world, PlayerEntity player, CraftingInventory inventory, CraftResultInventory inventoryResult, CraftingResultSlot craftingResultSlot) {
		if (!world.isClientSide) {
			ServerPlayerEntity serverplayerentity = (ServerPlayerEntity) player;
			ItemStack itemstack = ItemStack.EMPTY;
			if (lastRecipe != null && lastRecipe.matches(inventory, world)) {
				itemstack = lastRecipe.assemble(inventory);
			} else {
				//noinspection ConstantConditions - we're on server and for sure in the world so getServer can't return null here
				Optional<ICraftingRecipe> optional = world.getServer().getRecipeManager().getRecipeFor(IRecipeType.CRAFTING, inventory, world);
				if (optional.isPresent()) {
					ICraftingRecipe craftingRecipe = optional.get();
					if (inventoryResult.setRecipeUsed(world, serverplayerentity, craftingRecipe)) {
						lastRecipe = craftingRecipe;
						itemstack = lastRecipe.assemble(inventory);
					} else {
						lastRecipe = null;
					}
				}
			}

			craftingResultSlot.set(itemstack);
			if (serverplayerentity.containerMenu instanceof BackpackContainer) {
				((BackpackContainer) serverplayerentity.containerMenu).setSlotStackToUpdate(craftingResultSlot.index, itemstack);
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
			ItemStack slotStack = slot.getItem();
			slotStack.getItem().onCraftedBy(slotStack, player.level, player);
			return slotStack;
		}
		return super.getSlotStackToTransfer(slot);
	}

	@Override
	public void onTakeFromSlot(Slot slot, PlayerEntity player, ItemStack slotStack) {
		ItemStack remainder = slot.onTake(player, slotStack);
		if (!remainder.isEmpty()) {
			player.drop(remainder, false);
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
