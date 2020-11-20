package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.UpgradeDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public abstract class UpgradeContainerBase<T extends IUpgradeWrapper> implements IServerUpdater {
	protected final ArrayList<Slot> slots = new ArrayList<>();
	private final int containerId;
	protected final T upgradeWrapper;
	protected final boolean isClientSide;

	protected UpgradeContainerBase(int containerId, T upgradeWrapper, boolean isClientSide) {
		this.containerId = containerId;
		this.upgradeWrapper = upgradeWrapper;
		this.isClientSide = isClientSide;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public abstract UpgradeContainerType<T, ? extends UpgradeContainerBase<T>> getType();

	@Override
	public void sendBooleanToServer(String key, boolean value) {
		if (!isClientSide) {
			return;
		}
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), key, value));
	}

	@Override
	public void sendDataToServer(Supplier<CompoundNBT> supplyData) {
		if (!isClientSide) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putInt("containerId", containerId);
		PacketHandler.sendToServer(new UpgradeDataMessage(data));
	}

	public abstract void handleMessage(CompoundNBT data);

	public ItemStack getUpgradeStack() {
		return upgradeWrapper.getUpgradeStack();
	}
}
