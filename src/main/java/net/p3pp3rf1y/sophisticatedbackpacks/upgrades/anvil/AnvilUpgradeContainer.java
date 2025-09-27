package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.anvil;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.*;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SlotSuppliedHandler;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import javax.annotation.Nullable;

public class AnvilUpgradeContainer extends UpgradeContainerBase<AnvilUpgradeWrapper, AnvilUpgradeContainer> {
	private static final String DATA_SHIFT_CLICK_INTO_STORAGE = "shiftClickIntoStorage";
	private final Slot resultSlot;

	private final PersistableAnvilMenu anvilMenuDelegate;
	private Runnable nameChangeListener = () -> {};
	private boolean processingOnTakeLogic = false;
	public AnvilUpgradeContainer(Player player, int upgradeContainerId, AnvilUpgradeWrapper upgradeWrapper, UpgradeContainerType<AnvilUpgradeWrapper, AnvilUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		anvilMenuDelegate = new PersistableAnvilMenu(new Inventory(player));

		slots.add(anvilMenuDelegate.getSlot(0));
		slots.add(anvilMenuDelegate.getSlot(1));
		resultSlot = anvilMenuDelegate.getSlot(2);
		slots.add(resultSlot);
	}

	public void setNameChangeListener(Runnable nameChangeListener) {
		this.nameChangeListener = nameChangeListener;
	}

	@Override
	public void handlePacket(CompoundTag data) {
		if (data.contains(DATA_SHIFT_CLICK_INTO_STORAGE)) {
			setShiftClickIntoStorage(data.getBoolean(DATA_SHIFT_CLICK_INTO_STORAGE));
		} else if (data.contains("itemName")) {
			setItemName(data.getString("itemName"));
		}
	}

	@Override
	public void setUpgradeWrapper(IUpgradeWrapper updatedUpgradeWrapper) {
		super.setUpgradeWrapper(updatedUpgradeWrapper);
		anvilMenuDelegate.setItemName(upgradeWrapper.getItemName());
		anvilMenuDelegate.createResult();
		nameChangeListener.run();
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

	@Nullable
	public String getItemName() {
		return upgradeWrapper.getItemName();
	}

	private class PersistableAnvilMenu extends AnvilMenu {

		public PersistableAnvilMenu(Inventory playerInventory) {
			super(0, playerInventory, playerInventory.player.level().isClientSide() ? ContainerLevelAccess.NULL : ContainerLevelAccess.create(playerInventory.player.level(), playerInventory.player.blockPosition()));
			super.setItemName(upgradeWrapper.getItemName());
		}

		@Override
		protected void createInputSlots(ItemCombinerMenuSlotDefinition itemCombinerMenuSlotDefinition) {
			for(final ItemCombinerMenuSlotDefinition.SlotDefinition slotDefinition : itemCombinerMenuSlotDefinition.getSlots()) {
				this.addSlot(new SlotSuppliedHandler(() -> upgradeWrapper.getInventory(), slotDefinition.slotIndex(), 0, 0) {
					@Override
					public void setChanged() {
						super.setChanged();
						slotsChanged(inputSlots);
						if (slotDefinition.slotIndex() == 0) {
							if (upgradeWrapper.getItemName().isEmpty() != getItem().isEmpty()) {
								String newItemName = getItem().isEmpty() ? "" : getItem().getHoverName().getString();
								upgradeWrapper.setItemName(newItemName);
								setItemName(newItemName);
								nameChangeListener.run();
							}
							if (getItem().isEmpty()) {
								setItemName("");
								upgradeWrapper.setItemName("");
							}
						}
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
		public void slotsChanged(Container pInventory) {
			createResult();
		}

		@Override
		protected void onTake(Player player, ItemStack stack) {
			processingOnTakeLogic = true;
			super.onTake(player, stack);
			processingOnTakeLogic = false;
		}
	}
}
