package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public abstract class PickupUpgradeContainer extends UpgradeContainerBase {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";
	private final PickupUpgradeWrapper pickupWrapper;

	private PickupUpgradeContainer(int containerId, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide) {
		super(containerId, upgrade, isClientSide);
		pickupWrapper = new PickupUpgradeWrapper(upgrade, upgradeSaveHandler);
		ItemStackHandler filterHandler = pickupWrapper.getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> slots.add(new FilterSlotItemHandler(filterHandler, slot, -100, -100)));
	}

	public boolean isAllowList() {
		return pickupWrapper.isAllowList();
	}

	public boolean shouldMatchDurability() {
		return pickupWrapper.shouldMatchDurability();
	}

	public boolean shouldMatchNbt() {
		return pickupWrapper.shouldMatchNbt();
	}

	public PrimaryMatch getPrimaryMatch() {
		return pickupWrapper.getPrimaryMatch();
	}

	public void setAllowList(boolean isAllowList) {
		pickupWrapper.setAllowList(isAllowList);
		sendBooleanToServer(DATA_IS_ALLOW_LIST, isAllowList);
	}

	public void setMatchDurability(boolean matchDurability) {
		pickupWrapper.setMatchDurability(matchDurability);
		sendBooleanToServer(DATA_MATCH_DURABILITY, matchDurability);
	}

	public void setMatchNbt(boolean matchNbt) {
		pickupWrapper.setMatchNbt(matchNbt);
		sendBooleanToServer(DATA_MATCH_NBT, matchNbt);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		pickupWrapper.setPrimaryMatch(primaryMatch);
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

		public static final UpgradeContainerType<PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Basic::new);

		private Basic(int containerId, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide) {
			super(containerId, upgrade, upgradeSaveHandler, isClientSide);
		}

		@Override
		public UpgradeContainerType<?> getType() {
			return TYPE;
		}
	}

	public static class Advanced extends PickupUpgradeContainer {
		public static final UpgradeContainerType<PickupUpgradeContainer> TYPE = new UpgradeContainerType<>(Advanced::new);

		private Advanced(int containerId, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, boolean isClientSide) {
			super(containerId, upgrade, upgradeSaveHandler, isClientSide);
		}

		@Override
		public UpgradeContainerType<?> getType() {
			return TYPE;
		}
	}
}
