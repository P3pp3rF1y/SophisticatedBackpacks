package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.LongNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraft.world.World;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IPickupResponseUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedbackpacks.util.FilterItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.util.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class PickupUpgradeWrapper implements IUpgradeWrapper, IPickupResponseUpgrade {
	private static final int FULL_COOLDOWN = 60;

	private final ItemStack upgrade;
	private final PickupUpgradeItem upgradeItem;
	private final Consumer<ItemStack> upgradeSaveHandler;
	private FilterItemStackHandler filterHandler = null;

	public PickupUpgradeWrapper(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		this.upgrade = upgrade;
		upgradeItem = upgrade.getItem() instanceof PickupUpgradeItem ? (PickupUpgradeItem) upgrade.getItem() : ModItems.PICKUP_UPGRADE;
		this.upgradeSaveHandler = upgradeSaveHandler;
	}

	public ItemStackHandler getFilterHandler() {
		if (filterHandler == null) {
			filterHandler = new FilterItemStackHandler(getFilterSlotCount()) {
				@Override
				protected void onContentsChanged(int slot) {
					super.onContentsChanged(slot);
					upgrade.setTagInfo("filters", serializeNBT());
					save();
				}
			};
			NBTHelper.getCompound(upgrade, "filters").ifPresent(filterHandler::deserializeNBT);
		}

		return filterHandler;
	}

	private boolean matchesFilter(ItemStack stack) {
		if (isAllowList()) {
			return InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> stackMatchesFilter(stack, filter), () -> false, returnValue -> returnValue);
		} else {
			return InventoryHelper.iterate(getFilterHandler(), (slot, filter) -> !stackMatchesFilter(stack, filter), () -> true, returnValue -> !returnValue);
		}
	}

	@Override
	public ItemStack pickup(World world, ItemStack stack, IBackpackWrapper backpackWrapper, boolean simulate) {
		if (!matchesFilter(stack)) {
			return stack;
		}
		int originalCount = stack.getCount();
		ItemStack ret = InventoryHelper.insertIntoInventory(stack, backpackWrapper.getInventoryHandler(), simulate);
		if (originalCount == ret.getCount()) {
			setCooldown(world.getGameTime() + FULL_COOLDOWN);
		}

		return ret;
	}

	private void setCooldown(long time) {
		upgrade.setTagInfo("cooldownTime", LongNBT.valueOf(time));
	}

	@Override
	public long getCooldownTime() {
		return NBTHelper.getLong(upgrade, "cooldownTime").orElse(0L);
	}

	private boolean stackMatchesFilter(ItemStack stack, ItemStack filter) {
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
		} else if (primaryMatch == PrimaryMatch.TAGS && !anyTagMatches(stack, filter)) {
			return false;
		}

		if (shouldMatchDurability() && stack.getDamage() != filter.getDamage()) {
			return false;
		}

		return !shouldMatchNbt() || ItemStack.areItemStackTagsEqual(stack, filter);
	}

	private boolean anyTagMatches(ItemStack stack, ItemStack filter) {
		for (ResourceLocation tag : stack.getItem().getTags()) {
			if (filter.getItem().getTags().contains(tag)) {
				return true;
			}
		}
		return false;
	}

	public void setAllowList(boolean isAllowList) {
		NBTHelper.setBoolean(upgrade, "isAllowList", isAllowList);
		save();
	}

	public boolean isAllowList() {
		return NBTHelper.getBoolean(upgrade, "isAllowList").orElse(false);
	}

	private void save() {
		upgradeSaveHandler.accept(upgrade);
	}

	public boolean shouldMatchDurability() {
		return NBTHelper.getBoolean(upgrade, "matchDurability").orElse(false);
	}

	public void setMatchDurability(boolean matchDurability) {
		NBTHelper.setBoolean(upgrade, "matchDurability", matchDurability);
		save();
	}

	public void setMatchNbt(boolean matchNbt) {
		NBTHelper.setBoolean(upgrade, "matchNbt", matchNbt);
		save();
	}

	public boolean shouldMatchNbt() {
		return NBTHelper.getBoolean(upgrade, "matchNbt").orElse(false);
	}

	public void setPrimaryMatch(PrimaryMatch primaryMatch) {
		NBTHelper.setEnumConstant(upgrade, "primaryMatch", primaryMatch);
		save();
	}

	public PrimaryMatch getPrimaryMatch() {
		return NBTHelper.getEnumConstant(upgrade, "primaryMatch", PrimaryMatch::fromName).orElse(PrimaryMatch.ITEM);
	}

	public int getFilterSlotCount() {
		return upgradeItem.getFilterSlotCount();
	}

	@Override
	public ItemStack getUpgradeStack() {
		return upgrade;
	}
}
