package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public abstract class PickupUpgradeContainer extends UpgradeContainerBase<PickupUpgradeWrapper> {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";

	private PickupUpgradeContainer(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
		super(containerId, wrapper, isClientSide);
		ItemStackHandler filterHandler = upgradeWrapper.getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> slots.add(new FilterSlotItemHandler(filterHandler, slot, -100, -100)));
	}

	public boolean isAllowList() {
		return upgradeWrapper.isAllowList();
	}

	public boolean shouldMatchDurability() {
		return upgradeWrapper.shouldMatchDurability();
	}

	public boolean shouldMatchNbt() {
		return upgradeWrapper.shouldMatchNbt();
	}

	public PrimaryMatch getPrimaryMatch() {
		return upgradeWrapper.getPrimaryMatch();
	}

	public void setAllowList(boolean isAllowList) {
		upgradeWrapper.setAllowList(isAllowList);
		sendBooleanToServer(DATA_IS_ALLOW_LIST, isAllowList);
	}

	public void setMatchDurability(boolean matchDurability) {
		upgradeWrapper.setMatchDurability(matchDurability);
		sendBooleanToServer(DATA_MATCH_DURABILITY, matchDurability);
	}

	public void setMatchNbt(boolean matchNbt) {
		upgradeWrapper.setMatchNbt(matchNbt);
		sendBooleanToServer(DATA_MATCH_NBT, matchNbt);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		upgradeWrapper.setPrimaryMatch(primaryMatch);
		sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_PRIMARY_MATCH, primaryMatch));
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		for (String key : data.keySet()) {
			switch (key) {
				case DATA_IS_ALLOW_LIST:
					setAllowList(data.getBoolean(DATA_IS_ALLOW_LIST));
					break;
				case DATA_MATCH_DURABILITY:
					setMatchDurability(data.getBoolean(DATA_MATCH_DURABILITY));
					break;
				case DATA_MATCH_NBT:
					setMatchNbt(data.getBoolean(DATA_MATCH_NBT));
					break;
				case DATA_PRIMARY_MATCH:
					setPrimaryMatch(PrimaryMatch.fromName(data.getString(DATA_PRIMARY_MATCH)));
					break;
				default:
			}
		}

	}

	public static class Basic extends PickupUpgradeContainer {

		public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Basic::new);

		private Basic(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> getType() {
			return TYPE;
		}
	}

	public static class Advanced extends PickupUpgradeContainer {
		public static final UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Advanced::new);

		private Advanced(int containerId, PickupUpgradeWrapper wrapper, boolean isClientSide) {
			super(containerId, wrapper, isClientSide);
		}

		@Override
		public UpgradeContainerType<PickupUpgradeWrapper, PickupUpgradeContainer> getType() {
			return TYPE;
		}
	}
}
