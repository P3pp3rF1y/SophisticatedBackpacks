package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.toolswapper;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.world.Container;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.INameableEmptySlot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.IFilterSlot;

import javax.annotation.Nullable;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToolFilterSlot extends Slot implements IFilterSlot, INameableEmptySlot {
	private static final Container EMPTY_INVENTORY = new SimpleContainer(0);
	private final Supplier<ItemStack> getTool;
	private final Consumer<ItemStack> setTool;
	private final Predicate<ItemStack> isToolTypeValid;
	@Nullable
	private Component emptyTooltip;

	public ToolFilterSlot(Supplier<ItemStack> getTool, Consumer<ItemStack> setTool, Predicate<ItemStack> isToolTypeValid) {
		super(EMPTY_INVENTORY, 0, -100, -100);
		this.getTool = getTool;
		this.setTool = setTool;
		this.isToolTypeValid = isToolTypeValid;
	}

	@Override
	public void onQuickCraft(ItemStack oldStackIn, ItemStack newStackIn) {
		//noop
	}

	@Override
	public ItemStack getItem() {
		return getTool.get();
	}

	@Override
	public void set(ItemStack stack) {
		setTool.accept(stack);
		setChanged();
	}

	@Override
	public void setChanged() {
		//noop
	}

	@Override
	public int getMaxStackSize() {
		return 1;
	}

	@Override
	public ItemStack remove(int amount) {
		if (amount > 0) {
			set(ItemStack.EMPTY);
		}
		return getItem();
	}

	@Override
	public boolean mayPickup(Player playerIn) {
		return false;
	}

	@Override
	public boolean isSameInventory(Slot other) {
		return false;
	}

	@Override
	public boolean mayPlace(ItemStack stack) {
		return stack.isEmpty() || (stack.getMaxStackSize() == 1 && isToolTypeValid.test(stack));
	}

	@Override
	public boolean hasEmptyTooltip() {
		return emptyTooltip != null;
	}

	@Override
	public Component getEmptyTooltip() {
		return emptyTooltip == null ? new TextComponent("") : emptyTooltip;
	}

	public void setEmptyTooltip(String tooltip) {
		emptyTooltip = new TextComponent(tooltip);
	}
}
