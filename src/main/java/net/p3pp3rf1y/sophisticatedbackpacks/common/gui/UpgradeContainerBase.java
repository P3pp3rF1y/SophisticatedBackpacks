package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.ServerUpgradeDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public abstract class UpgradeContainerBase<W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>> implements IServerUpdater {
	protected final ArrayList<Slot> slots = new ArrayList<>();
	private final int containerId;
	protected W upgradeWrapper;
	protected final boolean isClientSide;
	private final UpgradeContainerType<W, C> type;

	protected UpgradeContainerBase(int containerId, W upgradeWrapper, boolean isClientSide, UpgradeContainerType<W, C> type) {
		this.containerId = containerId;
		this.upgradeWrapper = upgradeWrapper;
		this.isClientSide = isClientSide;
		this.type = type;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public UpgradeContainerType<W, C> getType() {
		return type;
	}

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
		PacketHandler.sendToServer(new ServerUpgradeDataMessage(data));
	}

	public abstract void handleMessage(CompoundNBT data);

	public ItemStack getUpgradeStack() {
		return upgradeWrapper.getUpgradeStack();
	}

	public W getUpgradeWrapper() {
		return upgradeWrapper;
	}

	public void setUpgradeWrapper(IUpgradeWrapper updatedUpgradeWrapper) {
		//noinspection unchecked - only used in logic that makes sure the item is the same and the same item will have a wrapper with the same (W) class
		upgradeWrapper = (W) updatedUpgradeWrapper;
	}
}
