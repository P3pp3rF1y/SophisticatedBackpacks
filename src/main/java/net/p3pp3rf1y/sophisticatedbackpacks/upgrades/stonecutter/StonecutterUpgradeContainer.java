package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.ContainerLevelAccess;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class StonecutterUpgradeContainer extends UpgradeContainerBase<StonecutterUpgradeWrapper, StonecutterUpgradeContainer> {
	private static final String DATA_SHIFT_CLICK_INTO_BACKPACK = "shiftClickIntoBackpack";
	private final StonecutterRecipeContainer recipeContainer;

	public StonecutterUpgradeContainer(Player player, int upgradeContainerId, StonecutterUpgradeWrapper upgradeWrapper, UpgradeContainerType<StonecutterUpgradeWrapper, StonecutterUpgradeContainer> type) {
		super(player, upgradeContainerId, upgradeWrapper, type);
		ContainerLevelAccess worldPosCallable = player.level.isClientSide ? ContainerLevelAccess.NULL : ContainerLevelAccess.create(player.level, player.blockPosition());
		recipeContainer = new StonecutterRecipeContainer(this, slots::add, this, worldPosCallable);
	}

	@Override
	public void handleMessage(CompoundTag data) {
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
		sendDataToServer(() -> NBTHelper.putBoolean(new CompoundTag(), DATA_SHIFT_CLICK_INTO_BACKPACK, shiftClickIntoBackpack));
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
