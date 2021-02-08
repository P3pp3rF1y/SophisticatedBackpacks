package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.inventory.container.Slot;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class DepositFilterLogicContainer extends FilterLogicContainer<DepositFilterLogic> {
	private static final String DATA_DEPOSIT_FILTER_TYPE = "depositFilterType";

	public DepositFilterLogicContainer(Supplier<DepositFilterLogic> filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		super(filterLogic, serverUpdater, addSlot);
		if (getDepositFilterType() == DepositFilterType.INVENTORY) {
			getFilterSlots().forEach(s -> s.setEnabled(false));
		}
	}

	public void setDepositFilterType(DepositFilterType depositFilterType) {
		filterLogic.get().setDepositFilterType(depositFilterType);
		serverUpdater.sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_DEPOSIT_FILTER_TYPE, depositFilterType));
	}

	@Override
	public boolean handleMessage(CompoundNBT data) {
		if (data.contains(DATA_DEPOSIT_FILTER_TYPE)) {
			setDepositFilterType(DepositFilterType.fromName(data.getString(DATA_DEPOSIT_FILTER_TYPE)));
		}
		return super.handleMessage(data);
	}

	public DepositFilterType getDepositFilterType() {
		return filterLogic.get().getDepositFilterType();
	}
}
