package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.UpgradeDataMessage;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public abstract class UpgradeContainerBase {
	protected final ArrayList<Slot> slots = new ArrayList<>();
	private final String containerId;
	protected final boolean isClientSide;

	public UpgradeContainerBase(ItemStack upgrade, boolean isClientSide) {
		//noinspection ConstantConditions
		containerId = upgrade.getItem().getRegistryName().toString();
		this.isClientSide = isClientSide;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public abstract UpgradeContainerType<?> getType();

	protected void sendDataToServer(Supplier<CompoundNBT> supplyData) {
		if (!isClientSide) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putString("containerId", containerId);
		PacketHandler.sendToServer(new UpgradeDataMessage(data));
	}

	public void handleMessage(@SuppressWarnings("unused") CompoundNBT data) {
		//noop
	}
}
