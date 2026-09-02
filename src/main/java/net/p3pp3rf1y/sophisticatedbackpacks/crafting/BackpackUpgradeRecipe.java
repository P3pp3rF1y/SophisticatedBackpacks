package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.inventory.CraftingContainer;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.ShapedRecipe;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageEndpointData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageGroupsSavedData;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

public class BackpackUpgradeRecipe extends ShapedRecipe implements IWrapperRecipe<ShapedRecipe> {
	public static final Set<ResourceLocation> REGISTERED_RECIPES = new LinkedHashSet<>();
	private final ShapedRecipe compose;

	public BackpackUpgradeRecipe(ShapedRecipe compose) {
		super(compose.getId(), compose.getGroup(), compose.category(), compose.getRecipeWidth(), compose.getRecipeHeight(), compose.getIngredients(),
				compose.result);
		this.compose = compose;
		REGISTERED_RECIPES.add(compose.getId());
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
	public boolean matches(CraftingContainer inv, Level level) {
		return super.matches(inv, level) && getBackpack(inv).map(backpack -> canUpgrade(backpack, level)).orElse(false);
	}

	static boolean canUpgrade(ItemStack backpack, Level level) {
		LinkedStorageEndpointData endpoint = LinkedStorageStackData.getEndpoint(backpack);
		if (endpoint == null) {
			return true;
		}
		return level instanceof net.minecraft.server.level.ServerLevel serverLevel
				? LinkedStorageGroupsSavedData.get(serverLevel).manager().isPrimaryEndpoint(endpoint.groupId(), endpoint.endpointId())
				: LinkedStorageStackData.isPrimaryEndpoint(backpack);
	}

	@Override
	public ItemStack assemble(CraftingContainer inv, RegistryAccess registryAccess) {
		ItemStack upgradedBackpack = super.assemble(inv, registryAccess);
		getBackpack(inv).flatMap(backpack -> Optional.ofNullable(backpack.getTag())).ifPresent(tag -> upgradedBackpack.setTag(tag.copy()));
		BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
		if (LinkedStorageStackData.getEndpoint(upgradedBackpack) != null) {
			upgradedBackpack.getOrCreateTag().putInt("inventorySlots", backpackItem.getNumberOfSlots());
			upgradedBackpack.getOrCreateTag().putInt("upgradeSlots", backpackItem.getNumberOfUpgradeSlots());
		} else {
			upgradedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots()));
		}

		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(CraftingContainer inv) {
		for (int slot = 0; slot < inv.getContainerSize(); slot++) {
			ItemStack slotStack = inv.getItem(slot);
			if (slotStack.getItem() instanceof BackpackItem) {
				return Optional.of(slotStack);
			}
		}

		return Optional.empty();
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return ModItems.BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	public static class Serializer extends RecipeWrapperSerializer<ShapedRecipe, BackpackUpgradeRecipe> {
		public Serializer() {
			super(BackpackUpgradeRecipe::new, RecipeSerializer.SHAPED_RECIPE);
		}
	}
}
