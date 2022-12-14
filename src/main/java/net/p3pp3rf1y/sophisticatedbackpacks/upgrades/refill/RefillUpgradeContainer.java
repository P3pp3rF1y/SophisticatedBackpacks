package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.refill;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Map;
import java.util.Optional;

public class RefillUpgradeContainer extends UpgradeContainerBase<RefillUpgradeWrapper, RefillUpgradeContainer> {
	private static final String DATA_SET_TARGET_SLOT = "setTargetSlot";
	private final FilterLogicContainer<FilterLogic> filterLogicContainer;

	public RefillUpgradeContainer(Player player, int containerId, RefillUpgradeWrapper wrapper, UpgradeContainerType<RefillUpgradeWrapper, RefillUpgradeContainer> type) {
		super(player, containerId, wrapper, type);

		filterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFilterLogic(), this, slots::add);
	}

	public FilterLogicContainer<FilterLogic> getFilterLogicContainer() {
		return filterLogicContainer;
	}

	public void setTargetSlot(int slot, RefillUpgradeWrapper.TargetSlot targetSlot) {
		upgradeWrapper.setTargetSlot(slot, targetSlot);
		sendDataToServer(() -> {
			CompoundTag tag = new CompoundTag();
			tag.put(DATA_SET_TARGET_SLOT, NBTHelper.putInt(NBTHelper.putEnumConstant(new CompoundTag(), "targetSlot", targetSlot), "slot", slot));
			return tag;
		});
	}

	public Map<Integer, RefillUpgradeWrapper.TargetSlot> getTargetSlots() {
		return upgradeWrapper.getTargetSlots();
	}

	public RefillUpgradeWrapper.TargetSlot getTargetSlot(int slot) {
		RefillUpgradeWrapper.TargetSlot targetSlot = upgradeWrapper.getTargetSlots().get(slot);
		return targetSlot != null ? targetSlot : RefillUpgradeWrapper.TargetSlot.ANY;
	}

	@Override
	public void handleMessage(CompoundTag data) {
		filterLogicContainer.handleMessage(data);
		if (data.contains(DATA_SET_TARGET_SLOT)) {
			CompoundTag tag = data.getCompound(DATA_SET_TARGET_SLOT);
			Optional<Integer> slot = NBTHelper.getInt(tag, "slot");
			Optional<RefillUpgradeWrapper.TargetSlot> targetSlot = NBTHelper.getEnumConstant(tag, "targetSlot", RefillUpgradeWrapper.TargetSlot::fromName);
			if (slot.isPresent() && targetSlot.isPresent()) {
				setTargetSlot(slot.get(), targetSlot.get());
			}
		}
	}

	public boolean allowsTargetSlotSelection() {
		return upgradeWrapper.allowsTargetSlotSelection();
	}
}
