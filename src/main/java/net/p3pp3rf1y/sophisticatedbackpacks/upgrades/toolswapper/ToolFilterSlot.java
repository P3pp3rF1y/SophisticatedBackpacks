package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.IInventory;
import net.minecraft.inventory.Inventory;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.StringTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.INameableEmptySlot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IFilterSlot;

import javax.annotation.Nullable;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToolFilterSlot extends Slot implements IFilterSlot, INameableEmptySlot {
	private static final IInventory EMPTY_INVENTORY = new Inventory(0);
	private final Supplier<ItemStack> getTool;
	private final Consumer<ItemStack> setTool;
	private final Predicate<ItemStack> isToolTypeValid;
	@Nullable
	private ITextComponent emptyTooltip;

	public ToolFilterSlot(Supplier<ItemStack> getTool, Consumer<ItemStack> setTool, Predicate<ItemStack> isToolTypeValid) {
		super(EMPTY_INVENTORY, 0, -100, -100);
		this.getTool = getTool;
		this.setTool = setTool;
		this.isToolTypeValid = isToolTypeValid;
	}

	@Override
	public void onSlotChange(ItemStack oldStackIn, ItemStack newStackIn) {
		//noop
	}

	@Override
	public ItemStack getStack() {
		return getTool.get();
	}

	@Override
	public void putStack(ItemStack stack) {
		setTool.accept(stack);
		onSlotChanged();
	}

	@Override
	public void onSlotChanged() {
		//noop
	}

	@Override
	public int getSlotStackLimit() {
		return 1;
	}

	@Override
	public ItemStack decrStackSize(int amount) {
		if (amount > 0) {
			putStack(ItemStack.EMPTY);
		}
		return getStack();
	}

	@Override
	public boolean canTakeStack(PlayerEntity playerIn) {
		return false;
	}

	@Override
	public boolean isSameInventory(Slot other) {
		return false;
	}

	@Override
	public boolean isItemValid(ItemStack stack) {
		return stack.isEmpty() || (stack.getMaxStackSize() == 1 && isToolTypeValid.test(stack));
	}

	@Override
	public boolean hasEmptyTooltip() {
		return emptyTooltip != null;
	}

	@Override
	public ITextComponent getEmptyTooltip() {
		return emptyTooltip == null ? new StringTextComponent("") : emptyTooltip;
	}

	public void setEmptyTooltip(String tooltip) {
		emptyTooltip = new StringTextComponent(tooltip);
	}
}
