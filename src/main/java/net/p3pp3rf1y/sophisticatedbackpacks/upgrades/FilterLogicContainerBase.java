package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.inventory.container.Slot;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Supplier;

public class FilterLogicContainerBase<T extends FilterLogicBase, S extends Slot> {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";

	protected final List<S> filterSlots = new ArrayList<>();
	protected final IServerUpdater serverUpdater;
	protected final Supplier<T> filterLogic;

	public FilterLogicContainerBase(IServerUpdater serverUpdater, Supplier<T> filterLogic) {
		this.serverUpdater = serverUpdater;
		this.filterLogic = filterLogic;
	}

	public List<S> getFilterSlots() {
		return filterSlots;
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
		for (String key : data.getAllKeys()) {
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
}
