package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.FilterSlotItemHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nonnull;
import java.util.function.Consumer;

public class FilterLogicContainer {
	private static final String DATA_IS_ALLOW_LIST = "isAllowList";
	private static final String DATA_MATCH_DURABILITY = "matchDurability";
	private static final String DATA_MATCH_NBT = "matchNbt";
	private static final String DATA_PRIMARY_MATCH = "primaryMatch";
	private final IServerUpdater serverUpdater;
	private final FilterLogic filterLogic;

	public FilterLogicContainer(FilterLogic filterLogic, IServerUpdater serverUpdater, Consumer<Slot> addSlot) {
		this.filterLogic = filterLogic;
		this.serverUpdater = serverUpdater;
		ItemStackHandler filterHandler = filterLogic.getFilterHandler();
		InventoryHelper.iterate(filterHandler, (slot, stack) -> addSlot.accept(new FilterSlotItemHandler(filterHandler, slot, -100, -100) {
			@Override
			public boolean isItemValid(@Nonnull ItemStack stack) {
				return filterHandler.isItemValid(slot, stack);
			}
		}));
	}

	public boolean isAllowList() {
		return filterLogic.isAllowList();
	}

	public boolean shouldMatchDurability() {
		return filterLogic.shouldMatchDurability();
	}

	public boolean shouldMatchNbt() {
		return filterLogic.shouldMatchNbt();
	}

	public PrimaryMatch getPrimaryMatch() {
		return filterLogic.getPrimaryMatch();
	}

	public void setAllowList(boolean isAllowList) {
		filterLogic.setAllowList(isAllowList);
		serverUpdater.sendBooleanToServer(DATA_IS_ALLOW_LIST, isAllowList);
	}

	public void setMatchDurability(boolean matchDurability) {
		filterLogic.setMatchDurability(matchDurability);
		serverUpdater.sendBooleanToServer(DATA_MATCH_DURABILITY, matchDurability);
	}

	public void setMatchNbt(boolean matchNbt) {
		filterLogic.setMatchNbt(matchNbt);
		serverUpdater.sendBooleanToServer(DATA_MATCH_NBT, matchNbt);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		filterLogic.setPrimaryMatch(primaryMatch);
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

}
