package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.core.HolderLookup;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.CraftingInput;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointStackState;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackLifecycle;

import java.util.Optional;

public class BackpackUpgradeRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.group(), compose.category(), compose.pattern, compose.result);
		this.compose = compose;
	}

	@Override
	public ShapedRecipe getCompose() {
		return compose;
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public boolean matches(CraftingInput inv, Level level) {
		return super.matches(inv, level) && getBackpack(inv).map(backpack -> canUpgrade(backpack, level)).orElse(false);
	}

	@Override
	public ItemStack assemble(CraftingInput inv, HolderLookup.Provider registries) {
		ItemStack upgradedBackpack = super.assemble(inv, registries);
		getBackpack(inv).map(ItemStack::getComponentsPatch).ifPresent(upgradedBackpack::applyComponents);
		BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
		if (LinkedStorageStackLifecycle.classifyEndpoint(upgradedBackpack) == LinkedStorageEndpointStackState.ENDPOINT) {
			setSlotNumbers(upgradedBackpack, backpackItem);
		} else {
			IBackpackWrapper wrapper = BackpackWrapper.fromStack(upgradedBackpack);
			wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
		}

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingInput inv) {
		for (int slot = 0; slot < inv.size(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	static boolean canUpgrade(ItemStack backpack, Level level) {
		if (LinkedStorageStackLifecycle.classifyEndpoint(backpack) != LinkedStorageEndpointStackState.ENDPOINT) {
			return true;
		}
		LinkedStorageEndpointData endpoint = backpack.get(ModCoreDataComponents.LINKED_STORAGE_ENDPOINT);
		if (level instanceof ServerLevel serverLevel) {
			return LinkedStorageGroupsSavedData.get(serverLevel).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId());
		}
		return Boolean.TRUE.equals(backpack.get(ModCoreDataComponents.LINKED_STORAGE_PRIMARY_ENDPOINT));
	}

	static void setSlotNumbers(ItemStack backpack, BackpackItem backpackItem) {
		backpack.set(ModCoreDataComponents.NUMBER_OF_INVENTORY_SLOTS, backpackItem.getNumberOfSlots());
		backpack.set(ModCoreDataComponents.NUMBER_OF_UPGRADE_SLOTS, backpackItem.getNumberOfUpgradeSlots());
	}

	@Override
	public RecipeSerializer<BackpackUpgradeRecipe> getSerializer() {
		return ModItems.BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, BackpackUpgradeRecipe> {
		public Serializer() {
			super(BackpackUpgradeRecipe::new, RecipeSerializer.SHAPED_RECIPE);
		}
	}
}
