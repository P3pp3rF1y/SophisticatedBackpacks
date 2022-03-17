package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemHandlerHelper;
import net.p3pp3rf1y.sophisticatedcore.util.FilterItemStackHandler;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class
FilterLogic extends FilterLogicBase {
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
				public void deserializeNBT(CompoundTag nbt) {
					setSize(filterSlotCount);
					ListTag tagList = nbt.getList("Items", Tag.TAG_COMPOUND);
					for (int i = 0; i < tagList.size(); i++) {
						CompoundTag itemTags = tagList.getCompound(i);
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
			return anyTagMatches(stack.getTags());
		}
		return allTagsMatch(stack.getTags());
	}

	private boolean allTagsMatch(Stream<TagKey<Item>> tagsStream) {
		if (tagKeys == null) {
			initTags();
		}
		Set<TagKey<Item>> tags = tagsStream.collect(Collectors.toSet());
		for (TagKey<Item> tagName : tagKeys) {
			if (!tags.contains(tagName)) {
				return false;
			}
		}
		return true;
	}

	private boolean anyTagMatches(Stream<TagKey<Item>> tags) {
		if (tagKeys == null) {
			initTags();
		}
		return tags.anyMatch(t -> tagKeys.contains(t));
	}
}