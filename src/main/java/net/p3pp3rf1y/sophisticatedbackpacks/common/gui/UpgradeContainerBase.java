package net.p3pp3rf1y.sophisticatedbackpacks.common.gui;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.network.SyncContainerClientDataMessage;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public abstract class UpgradeContainerBase<W extends IUpgradeWrapper, C extends UpgradeContainerBase<W, C>> implements IServerUpdater {
	protected final ArrayList<Slot> slots = new ArrayList<>();

	private final int upgradeContainerId;

	protected W upgradeWrapper;
	protected final PlayerEntity player;
	private final UpgradeContainerType<W, C> type;
	private boolean isOpen = false;

	protected UpgradeContainerBase(PlayerEntity player, int upgradeContainerId, W upgradeWrapper, UpgradeContainerType<W, C> type) {
		this.upgradeContainerId = upgradeContainerId;
		this.upgradeWrapper = upgradeWrapper;
		this.player = player;
		this.type = type;
	}

	public List<Slot> getSlots() {
		return slots;
	}

	public UpgradeContainerType<W, C> getType() {
		return type;
	}

	public void setIsOpen(boolean isOpen) {
		this.isOpen = isOpen;
	}

	public boolean isOpen() {
		return isOpen;
	}

	@Override
	public void sendBooleanToServer(String key, boolean value) {
		if (!player.level.isClientSide) {
			return;
		}
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), key, value));
	}

	@Override
	public void sendDataToServer(Supplier<CompoundNBT> supplyData) {
		if (!player.level.isClientSide) {
			return;
		}
		CompoundNBT data = supplyData.get();
		data.putInt("containerId", upgradeContainerId);
		PacketHandler.sendToServer(new SyncContainerClientDataMessage(data));
	}

	public void onInit() {
		//noop by default
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

	public boolean containsSlot(Slot slot) {
		for (Slot containerSlot : slots) {
			if (containerSlot == slot) {
				return true;
			}
		}
		return false;
	}

	public ItemStack getSlotStackToTransfer(Slot slot) {
		return slot.getItem();
	}

	public void onTakeFromSlot(Slot slot, PlayerEntity player, ItemStack slotStack) {
		slot.onTake(player, slotStack);
	}

	@SuppressWarnings({"unused", "java:S1172"}) //parameter is used in overrides
	public boolean mergeIntoBackpackFirst(Slot slot) {
		return true;
	}

	@SuppressWarnings({"unused", "java:S1172"}) //parameter is used in overrides
	public boolean allowsPickupAll(Slot slot) {
		return true;
	}

	public int getUpgradeContainerId() {
		return upgradeContainerId;
	}
}
