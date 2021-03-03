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
import net.minecraft.network.play.server.SSetSlotPacket;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.ICraftingContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.List;
import java.util.Optional;

public class CraftingUpgradeContainer extends UpgradeContainerBase<CraftingUpgradeWrapper, CraftingUpgradeContainer> implements ICraftingContainer {
	private static final String DATA_SHIFT_CLICK_INTO_BACKPACK = "shiftClickIntoBackpack";
	private final CraftResultInventory craftResult = new CraftResultInventory();
	private final CraftingItemHandler craftMatrix;
	private final CraftingResultSlot craftingResultSlot;

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

	private static void updateCraftingResult(World world, PlayerEntity player, CraftingInventory inventory, CraftResultInventory inventoryResult, CraftingResultSlot craftingResultSlot) {
		if (!world.isRemote) {
			ServerPlayerEntity serverplayerentity = (ServerPlayerEntity) player;
			ItemStack itemstack = ItemStack.EMPTY;
			//noinspection ConstantConditions - we're on server and for sure in the world so getServer can't return null here
			Optional<ICraftingRecipe> optional = world.getServer().getRecipeManager().getRecipe(IRecipeType.CRAFTING, inventory, world);
			if (optional.isPresent()) {
				ICraftingRecipe icraftingrecipe = optional.get();
				if (inventoryResult.canUseRecipe(world, serverplayerentity, icraftingrecipe)) {
					itemstack = icraftingrecipe.getCraftingResult(inventory);
				}
			}

			craftingResultSlot.putStack(itemstack);
			if (serverplayerentity.openContainer instanceof BackpackContainer) {
				serverplayerentity.connection.sendPacket(new SSetSlotPacket(serverplayerentity.openContainer.windowId, craftingResultSlot.slotNumber, itemstack));
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
}
