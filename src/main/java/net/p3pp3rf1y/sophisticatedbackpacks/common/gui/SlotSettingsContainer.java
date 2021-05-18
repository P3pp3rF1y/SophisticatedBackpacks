package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import com.google.common.collect.Lists;
import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.entity.player.PlayerInventory;
import net.minecraft.entity.player.ServerPlayerEntity;
import net.minecraft.inventory.container.ClickType;
import net.minecraft.inventory.container.Container;
import net.minecraft.inventory.container.IContainerListener;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.network.PacketBuffer;
import net.minecraft.util.NonNullList;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.SlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackInventoryHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackBackgroundProperties;

import java.util.ArrayList;
import java.util.List;

import static net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems.SLOT_SETTINGS_CONTAINER_TYPE;

public class SlotSettingsContainer extends Container implements IContextAwareContainer, ISyncedContainer {
	private final PlayerEntity player;
	private final BackpackContext backpackContext;
	private final IBackpackWrapper backpackWrapper;
	private final BackpackBackgroundProperties backpackBackgroundProperties;

	private final List<Slot> backpackInventorySlots = new ArrayList<>();

	public NonNullList<ItemStack> ghostItemStacks = NonNullList.create();

	public final List<Slot> ghostSlots = Lists.newArrayList();

	protected SlotSettingsContainer(int windowId, PlayerEntity player, BackpackContext backpackContext) {
		super(SLOT_SETTINGS_CONTAINER_TYPE.get(), windowId);
		this.player = player;
		this.backpackContext = backpackContext;

		backpackWrapper = backpackContext.getBackpackWrapper(player);
		backpackBackgroundProperties = getNumberOfSlots() <= 81 ? BackpackBackgroundProperties.REGULAR : BackpackBackgroundProperties.WIDE;

		addBackpackInventorySlots();
	}

	private void addBackpackInventorySlots() {
		BackpackInventoryHandler inventoryHandler = backpackWrapper.getInventoryHandler();

		int slotIndex = 0;
		int yPosition = 18;

		while (slotIndex < inventoryHandler.getSlots()) {
			int lineIndex = slotIndex % getSlotsOnLine();
			int finalSlotIndex = slotIndex;
			backpackInventorySlots.add(addSlot(new ViewOnlyBackpackInventorySlot(inventoryHandler, finalSlotIndex, lineIndex, yPosition)));

			slotIndex++;
			if (slotIndex % getSlotsOnLine() == 0) {
				yPosition += 18;
			}
		}
	}

	@Override
	protected Slot addSlot(Slot slot) {
		slot.slotNumber = ghostSlots.size();
		ghostSlots.add(slot);
		ghostItemStacks.add(ItemStack.EMPTY);
		return slot;
	}

	@Override
	public void detectAndSendChanges() {
		for (int i = 0; i < ghostSlots.size(); ++i) {
			ItemStack itemstack = ghostSlots.get(i).getStack();
			ItemStack itemstack1 = ghostItemStacks.get(i);
			if (!ItemStack.areItemStacksEqual(itemstack1, itemstack)) {
				boolean clientStackChanged = !itemstack1.equals(itemstack, true);
				ItemStack itemstack2 = itemstack.copy();
				ghostItemStacks.set(i, itemstack2);

				if (clientStackChanged) {
					for (IContainerListener icontainerlistener : listeners) {
						icontainerlistener.sendSlotContents(this, i, itemstack2);
					}
				}
			}
		}
	}

	@Override
	public void addListener(IContainerListener listener) {
		if (listener instanceof ServerPlayerEntity && backpackWrapper.getInventoryHandler().getStackSizeMultiplier() > 1) {
			super.addListener(new HighStackCountListener((ServerPlayerEntity) listener));
			return;
		}
		super.addListener(listener);
	}

	@Override
	public Slot getSlot(int slotId) {
		return ghostSlots.get(slotId);
	}

	private int getSlotsOnLine() {
		return backpackBackgroundProperties.getSlotsOnLine();
	}

	public int getNumberOfSlots() {
		return backpackWrapper.getInventoryHandler().getSlots();
	}

	@Override
	public boolean canInteractWith(PlayerEntity player) {
		return true;
	}

	@Override
	public BackpackContext getBackpackContext() {
		return backpackContext;
	}

	public List<Slot> getBackpackInventorySlots() {
		return backpackInventorySlots;
	}

	@Override
	public void handleMessage(CompoundNBT data) {

	}

	@Override
	public ItemStack slotClick(int slotId, int dragType, ClickType clickTypeIn, PlayerEntity player) {
		//TODO add delegation to the open settings

		return ItemStack.EMPTY;
	}

	public BackpackBackgroundProperties getBackpackBackgroundProperties() {
		return backpackBackgroundProperties;
	}

	public static SlotSettingsContainer fromBuffer(int windowId, PlayerInventory playerInventory, PacketBuffer packetBuffer) {
		return new SlotSettingsContainer(windowId, playerInventory.player, BackpackContext.fromBuffer(packetBuffer));
	}

	private static class ViewOnlyBackpackInventorySlot extends SlotItemHandler {
		public ViewOnlyBackpackInventorySlot(IItemHandler inventoryHandler, int slotIndex, int lineIndex, int yPosition) {
			super(inventoryHandler, slotIndex, 8 + lineIndex * 18, yPosition);
		}

		@Override
		public boolean canTakeStack(PlayerEntity playerIn) {
			return false;
		}
	}

	public int getNumberOfRows() {
		return (int) Math.ceil((double) getNumberOfSlots() / getSlotsOnLine());
	}
}
