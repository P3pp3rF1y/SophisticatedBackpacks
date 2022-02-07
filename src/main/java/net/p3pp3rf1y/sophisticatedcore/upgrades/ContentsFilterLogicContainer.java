package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.inventory.Slot;
import net.p3pp3rf1y.sophisticatedcore.common.gui.IServerUpdater;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Supplier;

public class ContentsFilterLogicContainer extends FilterLogicContainer<ContentsFilterLogic> {
	private static final String DATA_CONTENTS_FILTER_TYPE = "contentsFilterType";

	public ContentsFilterLogicContainer(Supplier<ContentsFilterLogic> filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		super(filterLogic, serverUpdater, addSlot);
		if (getFilterType() == ContentsFilterType.STORAGE) {
			getFilterSlots().forEach(s -> s.setEnabled(false));
		}
	}

	public void setFilterType(ContentsFilterType depositFilterType) {
		filterLogic.get().setDepositFilterType(depositFilterType);
		serverUpdater.sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundTag(), DATA_CONTENTS_FILTER_TYPE, depositFilterType));
	}

	@Override
	public boolean handleMessage(CompoundTag data) {
		if (data.contains(DATA_CONTENTS_FILTER_TYPE)) {
			setFilterType(ContentsFilterType.fromName(data.getString(DATA_CONTENTS_FILTER_TYPE)));
		}
		return super.handleMessage(data);
	}

	public ContentsFilterType getFilterType() {
		return filterLogic.get().getFilterType();
	}
}
