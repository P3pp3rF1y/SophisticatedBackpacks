package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;

import java.util.ArrayList;
import java.util.List;

public abstract class UpgradeContainerBase {
	protected final ArrayList<Slot> slots = new ArrayList<>();

	public UpgradeContainerBase() {
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public abstract UpgradeContainerType<?> getType();
}
