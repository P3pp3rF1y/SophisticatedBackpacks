package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.inventory.container.Slot;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.IItemHandler;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class FilterLogicContainer {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";
	private final IServerUpdater serverUpdater;
	private final Supplier<FilterLogic> filterLogic;
	private final List<Slot> filterSlots = new ArrayList<>();

	public FilterLogicContainer(Supplier<FilterLogic> filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		this.filterLogic = filterLogic;
		this.serverUpdater = serverUpdater;
		ItemStackHandler filterHandler = filterLogic.get().getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> {
			FilterLogicSlot filterSlot = new FilterLogicSlot(() -> filterLogic.get().getFilterHandler(), slot);
			addSlot.accept(filterSlot);
			filterSlots.add(filterSlot);
		});
	}

	public boolean isAllowList() {
		return filterLogic.get().isAllowList();
	}

	public boolean shouldMatchDurability() {
		return filterLogic.get().shouldMatchDurability();
	}

	public boolean shouldMatchNbt() {
		return filterLogic.get().shouldMatchNbt();
	}

	public PrimaryMatch getPrimaryMatch() {
		return filterLogic.get().getPrimaryMatch();
	}

	public void setAllowList(boolean isAllowList) {
		filterLogic.get().setAllowList(isAllowList);
		serverUpdater.sendBooleanToServer(DATA_IS_ALLOW_LIST, isAllowList);
	}

	public void setMatchDurability(boolean matchDurability) {
		filterLogic.get().setMatchDurability(matchDurability);
		serverUpdater.sendBooleanToServer(DATA_MATCH_DURABILITY, matchDurability);
	}

	public void setMatchNbt(boolean matchNbt) {
		filterLogic.get().setMatchNbt(matchNbt);
		serverUpdater.sendBooleanToServer(DATA_MATCH_NBT, matchNbt);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		filterLogic.get().setPrimaryMatch(primaryMatch);
		serverUpdater.sendDataToServer(() -> NBTHelper.putEnumConstant(new CompoundNBT(), DATA_PRIMARY_MATCH, primaryMatch));
	}

	public boolean handleMessage(CompoundNBT data) {
		for (String key : data.keySet()) {
			switch (key) {
				case DATA_IS_ALLOW_LIST:
					setAllowList(data.getBoolean(DATA_IS_ALLOW_LIST));
					return true;
				case DATA_MATCH_DURABILITY:
					setMatchDurability(data.getBoolean(DATA_MATCH_DURABILITY));
					return true;
				case DATA_MATCH_NBT:
					setMatchNbt(data.getBoolean(DATA_MATCH_NBT));
					return true;
				case DATA_PRIMARY_MATCH:
					setPrimaryMatch(PrimaryMatch.fromName(data.getString(DATA_PRIMARY_MATCH)));
					return true;
				default:
			}
		}
		return false;
	}

	public List<Slot> getFilterSlots() {
		return filterSlots;
	}

	public static class FilterLogicSlot extends FilterSlotItemHandler {
		public FilterLogicSlot(Supplier<IItemHandler> filterHandler, Integer slot) {
			super(filterHandler, slot, -100, -100);
		}
	}
}
