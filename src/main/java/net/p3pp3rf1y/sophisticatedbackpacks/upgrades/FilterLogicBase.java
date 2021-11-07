package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.util.Constants;
import net.p3pp3rf1y.sophisticatedbackpacks.util.ItemStackHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.Optional;
import java.util.Set;
import java.util.TreeSet;
import java.util.function.Consumer;

public class FilterLogicBase {
	protected final ItemStack upgrade;
	protected final Consumer<ItemStack> saveHandler;
	protected final String parentTagKey;
	private boolean allowListDefault = false;
	@Nullable
	protected Set<ResourceLocation> tagNames = null;

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
		} else if (primaryMatch == PrimaryMatch.ITEM && !ItemStack.isSame(stack, filter)) {
			return false;
		}

		if (shouldMatchDurability() && stack.getDamageValue() != filter.getDamageValue()) {
			return false;
		}

		return !shouldMatchNbt() || ItemStackHelper.areItemStackTagsEqualIgnoreDurability(stack, filter);
	}

	public Set<ResourceLocation> getTagNames() {
		if (tagNames == null) {
			initTags();
		}
		return Collections.unmodifiableSet(tagNames);
	}

	public void addTagName(ResourceLocation tagName) {
		if (tagNames == null) {
			initTags();
		}
		tagNames.add(tagName);
		serializeTags();
	}

	private void serializeTags() {
		if (tagNames == null) {
			return;
		}
		NBTHelper.setList(upgrade, parentTagKey, "tags", tagNames, t -> StringNBT.valueOf(t.toString()));
	}

	public void removeTagName(ResourceLocation tagName) {
		if (tagNames == null) {
			initTags();
		}
		tagNames.remove(tagName);
		serializeTags();
	}

	protected void initTags() {
		tagNames = NBTHelper.getCollection(upgrade, parentTagKey, "tags", Constants.NBT.TAG_STRING,
				elementNbt -> Optional.of(new ResourceLocation(elementNbt.getAsString())), TreeSet::new).orElse(new TreeSet<>());
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

	public boolean shouldMatchAnyTag() {
		return NBTHelper.getBoolean(upgrade, parentTagKey, "matchAnyTag").orElse(true);
	}

	public void setMatchAnyTag(boolean matchAnyTag) {
		NBTHelper.setBoolean(upgrade, parentTagKey, "matchAnyTag", matchAnyTag);
		save();
	}
}
