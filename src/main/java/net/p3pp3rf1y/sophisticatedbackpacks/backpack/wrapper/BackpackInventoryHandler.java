package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.IntConsumer;

public class BackpackInventoryHandler extends ItemStackHandler {
	private static final String INVENTORY_TAG = "inventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;
	private final List<IntConsumer> onContentsChangedListeners = new ArrayList<>();

	public BackpackInventoryHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler) {
		super(getNumberOfSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		NBTHelper.getCompound(backpack, INVENTORY_TAG).ifPresent(this::deserializeNBT);
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
		return backpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance()).map(w -> w.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE)).orElse(false);
	}

	private boolean isBackpackWithoutInceptionUpgrade(ItemStack stack) {
		return (stack.getItem() instanceof BackpackItem) && !stack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
				.map(w -> w.getUpgradeHandler().hasUpgrade(InceptionUpgradeItem.TYPE)).orElse(false);
	}

	public void saveInventory() {
		backpack.setTagInfo(INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
	}

	private static int getNumberOfSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfSlots();
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
