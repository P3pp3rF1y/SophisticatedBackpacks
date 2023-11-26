package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AnvilMenu;
import net.minecraft.world.inventory.ContainerLevelAccess;
import net.minecraft.world.inventory.ResultSlot;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class AnvilUpgradeContainer extends UpgradeContainerBase<AnvilUpgradeWrapper, AnvilUpgradeContainer> {
	private static final String DATA_SHIFT_CLICK_INTO_STORAGE = "shiftClickIntoStorage";
	private final Slot resultSlot;

	private PersistableAnvilMenu anvilMenuDelegate;
	private Runnable slotsChangeListener = () -> {};
	private boolean processingOnTakeLogic = false;
	public AnvilUpgradeContainer(Player player, int upgradeContainerId, AnvilUpgradeWrapper upgradeWrapper, UpgradeContainerType<AnvilUpgradeWrapper, AnvilUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);

		anvilMenuDelegate = new PersistableAnvilMenu(new Inventory(player), upgradeWrapper);

		slots.add(anvilMenuDelegate.getSlot(0));
		slots.add(anvilMenuDelegate.getSlot(1));
		resultSlot = anvilMenuDelegate.getSlot(2);
		slots.add(resultSlot);
	}

	public void setSlotsChangeListener(Runnable slotsChangeListener) {
		this.slotsChangeListener = slotsChangeListener;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		if (data.contains(DATA_SHIFT_CLICK_INTO_STORAGE)) {
			setShiftClickIntoStorage(data.getBoolean(DATA_SHIFT_CLICK_INTO_STORAGE));
		} else if (data.contains("itemName")) {
			setItemName(data.getString("itemName"));
		}
	}

	public boolean shouldShiftClickIntoStorage() {
		return upgradeWrapper.shouldShiftClickIntoStorage();
	}

	public boolean isProcessingOnTakeLogic() {
		return processingOnTakeLogic;
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

	public void setItemName(String name) {
		anvilMenuDelegate.setItemName(name);
		upgradeWrapper.setItemName(name);
		sendDataToServer(() -> NBTHelper.putString(new CompoundTag(), "itemName", name));
	}

	public int getCost() {
		return anvilMenuDelegate.getCost();
	}

	public String getItemName() {
		return anvilMenuDelegate.getItemName();
	}

	private class PersistableAnvilMenu extends AnvilMenu {

		private final AnvilUpgradeWrapper wrapper;
		private boolean initializing = true;
		public PersistableAnvilMenu(Inventory playerInventory, AnvilUpgradeWrapper wrapper) {
			super(0, playerInventory, ContainerLevelAccess.create(playerInventory.player.level, playerInventory.player.blockPosition()));
			this.wrapper = wrapper;
			inputSlots.setItem(0, wrapper.getInventory().getStackInSlot(0));
			inputSlots.setItem(1, wrapper.getInventory().getStackInSlot(1));

			super.setItemName(wrapper.getItemName());
			initializing = false;
		}

		public String getItemName() {
			return itemName;
		}

		@Override
		public void slotsChanged(Container pInventory) {
			super.slotsChanged(pInventory);
			if (initializing) {
				return;
			}
			wrapper.getInventory().setStackInSlot(0, inputSlots.getItem(0));
			wrapper.getInventory().setStackInSlot(1, inputSlots.getItem(1));
			slotsChangeListener.run();
		}

		@Override
		protected void onTake(Player player, ItemStack stack) {
			processingOnTakeLogic = true;
			super.onTake(player, stack);
			processingOnTakeLogic = false;
		}
	}
}
