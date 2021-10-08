package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.util.Constants;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilterItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;

public class FilterLogic extends FilterLogicBase {
	private final int filterSlotCount;
	private final Predicate<ItemStack> isItemValid;
	private FilterItemStackHandler filterHandler = null;
	private boolean emptyAllowListMatchesEverything = false;

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount) {
		this(upgrade, saveHandler, filterSlotCount, s -> true, "");
	}

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Predicate<ItemStack> isItemValid) {
		this(upgrade, saveHandler, filterSlotCount, isItemValid, "");
	}

	public FilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, Predicate<ItemStack> isItemValid, String parentTagKey) {
		super(upgrade, saveHandler, parentTagKey);
		this.filterSlotCount = filterSlotCount;
		this.isItemValid = isItemValid;
	}

	public void setEmptyAllowListMatchesEverything() {
		emptyAllowListMatchesEverything = true;
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
							ItemStack stack = ItemStack.of(itemTags);
							stacks.set(slot, stack);
						}
					}
					onLoad();
				}

				@Override
				public boolean isItemValid(int slot, ItemStack stack) {
					return stack.isEmpty() || (doesNotContain(stack) && isItemValid.test(stack));
				}

				private boolean doesNotContain(ItemStack stack) {
					return !InventoryHelper.hasItem(this, s -> ItemHandlerHelper.canItemStacksStack(s, stack));
				}
			};
			NBTHelper.getCompound(upgrade, parentTagKey, "filters").ifPresent(filterHandler::deserializeNBT);
		}

		return filterHandler;
	}

	public boolean matchesFilter(ItemStack stack) {
		if (isAllowList()) {
			if (getPrimaryMatch() == PrimaryMatch.TAGS) {
				return isTagMatch(stack);
			} else {
				return (getFilterHandler().hasOnlyEmptyFilters() && emptyAllowListMatchesEverything)
						|| InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> stackMatchesFilter(stack, filter), () -> false, returnValue -> returnValue);
			}
		} else {
			if (getPrimaryMatch() == PrimaryMatch.TAGS) {
				return !isTagMatch(stack);
			} else {
				return getFilterHandler().hasOnlyEmptyFilters()
						|| InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> !stackMatchesFilter(stack, filter), () -> true, returnValue -> !returnValue);
			}
		}
	}

	private boolean isTagMatch(ItemStack stack) {
		if (shouldMatchAnyTag()) {
			return anyTagMatches(stack.getItem().getTags());
		}
		return allTagsMatch(stack.getItem().getTags());
	}

	private boolean allTagsMatch(Set<ResourceLocation> tags) {
		if (tagNames == null) {
			initTags();
		}
		for (ResourceLocation tagName : tagNames) {
			if (!tags.contains(tagName)) {
				return false;
			}
		}
		return true;
	}

	private boolean anyTagMatches(Set<ResourceLocation> tags) {
		if (tagNames == null) {
			initTags();
		}
		for (ResourceLocation tag : tags) {
			if (tagNames.contains(tag)) {
				return true;
			}
		}
		return false;
	}
}