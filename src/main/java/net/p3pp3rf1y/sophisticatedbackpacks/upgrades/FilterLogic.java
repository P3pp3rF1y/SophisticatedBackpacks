package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilterItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;
import java.util.function.Predicate;

public class FilterLogic {
	private final ItemStack upgrade;
	private final Consumer<ItemStack> saveHandler;
	private final int filterSlotCount;
	private final Predicate<ItemStack> isItemValid;
	private final String parentTagKey;
	private FilterItemStackHandler filterHandler = null;
	private boolean allowListDefault = false;
	private boolean emptyAllowListMatchesEverything = false;

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount) {
		this(upgrade, saveHandler, filterSlotCount, s -> true, "");
	}

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Predicate<ItemStack> isItemValid) {
		this(upgrade, saveHandler, filterSlotCount, isItemValid, "");
	}

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Predicate<ItemStack> isItemValid, String parentTagKey) {
		this.upgrade = upgrade;
		this.saveHandler = saveHandler;
		this.filterSlotCount = filterSlotCount;
		this.isItemValid = isItemValid;
		this.parentTagKey = parentTagKey;
	}

	public void setEmptyAllowListMatchesEverything() {
		emptyAllowListMatchesEverything = true;
	}

	public void setAllowByDefault() {
		allowListDefault = true;
	}

	public FilterItemStackHandler getFilterHandler() {
		if (filterHandler == null) {
			filterHandler = new FilterItemStackHandler(filterSlotCount) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					NBTHelper.setCompoundNBT(upgrade, parentTagKey, "filters", serializeNBT());
					save();
				}

				@Override
				public void deserializeNBT(CompoundNBT nbt) {
					setSize(filterSlotCount);
					ListNBT tagList = nbt.getList("Items", Constants.NBT.TAG_COMPOUND);
					for (int i = 0; i < tagList.size(); i++) {
						CompoundNBT itemTags = tagList.getCompound(i);
						int slot = itemTags.getInt("Slot");

						if (slot >= 0 && slot < stacks.size()) {
							ItemStack stack = ItemStack.read(itemTags);
							stacks.set(slot, stack);
						}
					}
					onLoad();
				}

				@Override
				public boolean isItemValid(int slot, ItemStack stack) {
					return stack.isEmpty() || isItemValid.test(stack);
				}
			};
			NBTHelper.getCompound(upgrade, parentTagKey, "filters").ifPresent(filterHandler::deserializeNBT);
		}

		return filterHandler;
	}

	private void save() {
		saveHandler.accept(upgrade);
	}

	public boolean matchesFilter(ItemStack stack) {
		if (isAllowList()) {
			return (getFilterHandler().hasOnlyEmptyFilters() && emptyAllowListMatchesEverything)
					|| InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> stackMatchesFilter(stack, filter), () -> false, returnValue -> returnValue);
		} else {
			return getFilterHandler().hasOnlyEmptyFilters()
					|| InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> !stackMatchesFilter(stack, filter), () -> true, returnValue -> !returnValue);
		}
	}

	public boolean stackMatchesFilter(ItemStack stack, ItemStack filter) {
		if (filter.isEmpty()) {
			return false;
		}

		PrimaryMatch primaryMatch = getPrimaryMatch();
		if (primaryMatch == PrimaryMatch.MOD) {
			//noinspection ConstantConditions
			if (!stack.getItem().getRegistryName().getNamespace().equals(filter.getItem().getRegistryName().getNamespace())) {
				return false;
			}
		} else if (primaryMatch == PrimaryMatch.ITEM) {
			if (!ItemStack.areItemsEqual(stack, filter)) {
				return false;
			}
		} else if (primaryMatch == PrimaryMatch.TAGS && !InventoryHelper.anyStackTagMatches(stack, filter)) {
			return false;
		}

		if (shouldMatchDurability() && stack.getDamage() != filter.getDamage()) {
			return false;
		}

		return !shouldMatchNbt() || ItemStack.areItemStackTagsEqual(stack, filter);
	}

	public void setAllowList(boolean isAllowList) {
		NBTHelper.setBoolean(upgrade, parentTagKey, "isAllowList", isAllowList);
		save();
	}

	public boolean isAllowList() {
		return NBTHelper.getBoolean(upgrade, parentTagKey, "isAllowList").orElse(allowListDefault);
	}

	public boolean shouldMatchDurability() {
		return NBTHelper.getBoolean(upgrade, parentTagKey, "matchDurability").orElse(false);
	}

	public void setMatchDurability(boolean matchDurability) {
		NBTHelper.setBoolean(upgrade, parentTagKey, "matchDurability", matchDurability);
		save();
	}

	public void setMatchNbt(boolean matchNbt) {
		NBTHelper.setBoolean(upgrade, parentTagKey, "matchNbt", matchNbt);
		save();
	}

	public boolean shouldMatchNbt() {
		return NBTHelper.getBoolean(upgrade, parentTagKey, "matchNbt").orElse(false);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		NBTHelper.setEnumConstant(upgrade, parentTagKey, "primaryMatch", primaryMatch);
		save();
	}

	public PrimaryMatch getPrimaryMatch() {
		return NBTHelper.getEnumConstant(upgrade, parentTagKey, "primaryMatch", PrimaryMatch::fromName).orElse(PrimaryMatch.ITEM);
	}
}