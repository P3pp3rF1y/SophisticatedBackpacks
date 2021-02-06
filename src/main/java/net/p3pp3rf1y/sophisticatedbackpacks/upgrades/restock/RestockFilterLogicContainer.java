package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.restock;

import net.minecraft.inventory.container.Slot;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.IServerUpdater;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class RestockFilterLogicContainer extends FilterLogicContainer<RestockFilterLogic> {
	private static final String DATA_RESTOCK_FILTER_TYPE = "restockFilterType";

	public RestockFilterLogicContainer(Supplier<RestockFilterLogic> filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		super(filterLogic, serverUpdater, addSlot);
		if (getRestockFilterType() == RestockFilterType.BACKPACK) {
			getFilterSlots().forEach(s -> s.setEnabled(false));
		}
	}

	public void setRestockFilterType(RestockFilterType depositFilterType) {
		filterLogic.get().setDepositFilterType(depositFilterType);
		serverUpdater.sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_RESTOCK_FILTER_TYPE, depositFilterType));
	}

	@Override
	public boolean handleMessage(CompoundNBT data) {
		if (data.contains(DATA_RESTOCK_FILTER_TYPE)) {
			setRestockFilterType(RestockFilterType.fromName(data.getString(DATA_RESTOCK_FILTER_TYPE)));
		}
		return super.handleMessage(data);
	}

	public RestockFilterType getRestockFilterType() {
		return filterLogic.get().getRestockFilterType();
	}
}
