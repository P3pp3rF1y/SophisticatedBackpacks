package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.IntConsumer;

public class BackpackInventoryHandler extends ItemStackHandler {
	public static final String INVENTORY_TAG = "inventory";
	private final IBackpackWrapper backpackWrapper;
	private final CompoundNBT contentsNbt;
	private final Runnable backpackSaveHandler;
	private final List<IntConsumer> onContentsChangedListeners = new ArrayList<>();
	private boolean persistent = true;
	private final Map<Integer, CompoundNBT> stackNbts = new LinkedHashMap<>();

	public BackpackInventoryHandler(int numberOfInventorySlots, IBackpackWrapper backpackWrapper, CompoundNBT contentsNbt, Runnable backpackSaveHandler) {
		super(numberOfInventorySlots);
		this.backpackWrapper = backpackWrapper;
		this.contentsNbt = contentsNbt;
		this.backpackSaveHandler = backpackSaveHandler;
		deserializeNBT(contentsNbt.getCompound(INVENTORY_TAG));
	}

	@Override
	public void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		if (persistent && updateSlotNbt(slot)) {
			saveInventory();
			onContentsChangedListeners.forEach(l -> l.accept(slot));
		}
	}

	private boolean updateSlotNbt(int slot) {
		ItemStack slotStack = getStackInSlot(slot);
		if (slotStack.isEmpty()) {
			if (stackNbts.containsKey(slot)) {
				stackNbts.remove(slot);
				return true;
			}
		} else {
			CompoundNBT itemTag = new CompoundNBT();
			itemTag.putInt("Slot", slot);
			slotStack.write(itemTag);
			if (!stackNbts.containsKey(slot) || !stackNbts.get(slot).equals(itemTag)) {
				stackNbts.put(slot, itemTag);
				return true;
			}
		}
		return false;
	}

	public void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return !(stack.getItem() instanceof BackpackItem) || (hasInceptionUpgrade() && isBackpackWithoutInceptionUpgrade(stack));
	}

	private boolean hasInceptionUpgrade() {
		return backpackWrapper.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE);
	}

	private boolean isBackpackWithoutInceptionUpgrade(ItemStack stack) {
		return (stack.getItem() instanceof BackpackItem) && !stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(w -> w.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE)).orElse(false);
	}

	public void saveInventory() {
		contentsNbt.put(INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.run();
	}

	public void copyStacksTo(BackpackInventoryHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public void addListener(IntConsumer onContentsChanged) {
		onContentsChangedListeners.add(onContentsChanged);
	}

	public void clearListeners() {
		onContentsChangedListeners.clear();
	}

	@Override
	public CompoundNBT serializeNBT() {
		ListNBT nbtTagList = new ListNBT();
		nbtTagList.addAll(stackNbts.values());
		CompoundNBT nbt = new CompoundNBT();
		nbt.put("Items", nbtTagList);
		nbt.putInt("Size", getSlots());
		return nbt;
	}
}
