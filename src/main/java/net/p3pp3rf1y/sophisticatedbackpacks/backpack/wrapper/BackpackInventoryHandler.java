package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.IntConsumer;

public class BackpackInventoryHandler extends ItemStackHandler {
	public static final String INVENTORY_TAG = "inventory";
	private final IBackpackWrapper backpackWrapper;
	private final CompoundNBT contentsNbt;
	private final Runnable backpackSaveHandler;
	private final List<IntConsumer> onContentsChangedListeners = new ArrayList<>();

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
		saveInventory();
		onContentsChangedListeners.forEach(l -> l.accept(slot));
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
}
