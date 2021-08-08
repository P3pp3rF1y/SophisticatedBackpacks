package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.inventory.container.Slot;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.util.IWorldPosCallable;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class StonecutterUpgradeContainer extends UpgradeContainerBase<StonecutterUpgradeWrapper, StonecutterUpgradeContainer> {
	private static final String DATA_SHIFT_CLICK_INTO_BACKPACK = "shiftClickIntoBackpack";
	private final StonecutterRecipeContainer recipeContainer;

	public StonecutterUpgradeContainer(PlayerEntity player, int upgradeContainerId, StonecutterUpgradeWrapper upgradeWrapper, UpgradeContainerType<StonecutterUpgradeWrapper, StonecutterUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		IWorldPosCallable worldPosCallable = player.level.isClientSide ? IWorldPosCallable.NULL : IWorldPosCallable.create(player.level, player.blockPosition());
		recipeContainer = new StonecutterRecipeContainer(this, slots::add, this, worldPosCallable);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		if (data.contains(DATA_SHIFT_CLICK_INTO_BACKPACK)) {
			setShiftClickIntoBackpack(data.getBoolean(DATA_SHIFT_CLICK_INTO_BACKPACK));
		} else {
			recipeContainer.handleMessage(data);
		}
	}

	public boolean shouldShiftClickIntoBackpack() {
		return upgradeWrapper.shouldShiftClickIntoBackpack();
	}

	public void setShiftClickIntoBackpack(boolean shiftClickIntoBackpack) {
		upgradeWrapper.setShiftClickIntoBackpack(shiftClickIntoBackpack);
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundNBT(), DATA_SHIFT_CLICK_INTO_BACKPACK, shiftClickIntoBackpack));
	}

	public StonecutterRecipeContainer getRecipeContainer() {
		return recipeContainer;
	}

	@Override
	public boolean mergeIntoBackpackFirst(Slot slot) {
		return recipeContainer.isNotResultSlot(slot) || shouldShiftClickIntoBackpack();
	}

	@Override
	public boolean allowsPickupAll(Slot slot) {
		return recipeContainer.isNotResultSlot(slot);
	}
}
