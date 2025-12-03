package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.deposit;

import net.minecraft.core.component.DataComponentMap;
import net.minecraft.core.component.DataComponentType;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.neoforged.neoforge.registries.DeferredHolder;
import net.neoforged.neoforge.transfer.ResourceHandler;
import net.neoforged.neoforge.transfer.item.ItemResource;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.inventory.ItemStackKey;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterAttributes;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

public class DepositFilterLogic extends FilterLogic {
	private Set<ItemStackKey> inventoryFilterStacks = new HashSet<>();

	public DepositFilterLogic(ItemStack upgrade, Consumer<ItemStack> saveHandler, int filterSlotCount, DeferredHolder<DataComponentType<?>, DataComponentType<FilterAttributes>> contentsComponent) {
		super(upgrade, saveHandler, filterSlotCount, contentsComponent);
	}

	public DepositFilterType getDepositFilterType() {
		if (shouldFilterByInventory()) {
			return DepositFilterType.INVENTORY;
		}
		return isAllowList() ? DepositFilterType.ALLOW : DepositFilterType.BLOCK;
	}

	public void setDepositFilterType(DepositFilterType depositFilterType) {
		switch (depositFilterType) {
			case ALLOW:
				setFilterByInventory(false);
				setAllowList(true);
				break;
			case BLOCK:
				setFilterByInventory(false);
				setAllowList(false);
				break;
			case INVENTORY:
			default:
				setFilterByInventory(true);
				save();
		}
	}

	public void setInventory(ResourceHandler<ItemResource> inventory) {
		inventoryFilterStacks = InventoryHelper.getUniqueStacks(inventory);
	}

	@Override
	protected boolean matchesFilter(Stream<TagKey<Item>> tags, Item item, int damageValue, boolean empty, DataComponentMap components) {
		if (!shouldFilterByInventory()) {
			return super.matchesFilter(tags, item, damageValue, empty, components);
		}

		for (ItemStackKey filterStack : inventoryFilterStacks) {
			if (stackMatchesFilter(filterStack.stack(), item, damageValue, empty, components)) {
				return true;
			}
		}
		return false;
	}

	private void setFilterByInventory(boolean filterByInventory) {
		upgrade.set(ModDataComponents.FILTER_BY_INVENTORY, filterByInventory);
		save();
	}

	private boolean shouldFilterByInventory() {
		return upgrade.getOrDefault(ModDataComponents.FILTER_BY_INVENTORY, false);
	}
}
