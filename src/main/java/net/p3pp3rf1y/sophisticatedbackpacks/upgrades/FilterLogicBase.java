package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Objects;
import java.util.function.Consumer;

public class FilterLogicBase {
	protected final ItemStack upgrade;
	protected final Consumer<ItemStack> saveHandler;
	protected final String parentTagKey;
	private boolean allowListDefault = false;

	public FilterLogicBase(ItemStack upgrade, Consumer<ItemStack> saveHandler, String parentTagKey) {
		this.upgrade = upgrade;
		this.saveHandler = saveHandler;
		this.parentTagKey = parentTagKey;
	}

	public void setAllowByDefault() {
		allowListDefault = true;
	}

	protected void save() {
		saveHandler.accept(upgrade);
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
			if (!ItemStack.isSame(stack, filter)) {
				return false;
			}
		} else if (primaryMatch == PrimaryMatch.TAGS && !InventoryHelper.anyStackTagMatches(stack, filter)) {
			return false;
		}

		if (shouldMatchDurability() && stack.getDamageValue() != filter.getDamageValue()) {
			return false;
		}

		return !shouldMatchNbt() || areItemStackTagsEqualIgnoreDurability(stack, filter);
	}

	private boolean areItemStackTagsEqualIgnoreDurability(ItemStack stackA, ItemStack stackB) {
		if (stackA.isEmpty() && stackB.isEmpty()) {
			return true;
		} else if (!stackA.isEmpty() && !stackB.isEmpty()) {
			if (stackA.getTag() == null && stackB.getTag() != null) {
				return false;
			} else {
				return (stackA.getTag() == null || areTagsEqualIgnoreDurability(stackA.getTag(), stackB.getTag())) && stackA.areCapsCompatible(stackB);
			}
		} else {
			return false;
		}
	}

	private boolean areTagsEqualIgnoreDurability(CompoundNBT tagA, @Nullable CompoundNBT tagB) {
		if (tagA == tagB) {
			return true;
		}
		if (tagB == null || tagA.size() != tagB.size()) {
			return false;
		}

		for (String key : tagA.getAllKeys()) {
			if (!tagB.contains(key)) {
				return false;
			}
			if (key.equals("Damage")) {
				continue;
			}
			if (!Objects.equals(tagA.get(key), tagB.get(key))) {
				return false;
			}
		}
		return true;
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
