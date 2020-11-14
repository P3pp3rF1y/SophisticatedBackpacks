package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.UpgradeDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public abstract class UpgradeContainerBase {
	protected final ArrayList<Slot> slots = new ArrayList<>();
	private final int containerId;
	private final ItemStack upgradeStack;
	protected final boolean isClientSide;

	public UpgradeContainerBase(int containerId, ItemStack upgrade, boolean isClientSide) {
		this.containerId = containerId;
		upgradeStack = upgrade;
		this.isClientSide = isClientSide;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public abstract UpgradeContainerType<?> getType();

	protected void sendBooleanToServer(String key, boolean value) {
		if (!isClientSide) {
			return;
		}
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), key, value));
	}

	protected void sendDataToServer(Supplier<CompoundNBT> supplyData) {
		if (!isClientSide) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putInt("containerId", containerId);
		PacketHandler.sendToServer(new UpgradeDataMessage(data));
	}

	public void handleMessage(@SuppressWarnings("unused") CompoundNBT data) {
		//noop
	}

	public ItemStack getUpgradeStack() {
		return upgradeStack;
	}
}
