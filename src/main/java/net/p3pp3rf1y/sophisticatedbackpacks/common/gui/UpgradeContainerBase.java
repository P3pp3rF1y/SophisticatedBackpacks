package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;

import java.util.ArrayList;
import java.util.List;

public abstract class UpgradeContainerBase {
	protected final ArrayList<Slot> slots = new ArrayList<>();
	protected ItemStack upgrade;

	public UpgradeContainerBase(ItemStack upgrade) {
		this.upgrade = upgrade;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public abstract UpgradeContainerType<?> getType();
}
