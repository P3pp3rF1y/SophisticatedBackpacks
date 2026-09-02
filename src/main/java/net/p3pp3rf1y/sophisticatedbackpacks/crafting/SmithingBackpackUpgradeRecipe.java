package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.core.RegistryAccess;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.Ingredient;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.SmithingTransformRecipe;
import net.minecraft.world.level.Level;
import net.minecraftforge.fml.util.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;
import net.p3pp3rf1y.sophisticatedcore.linkedstorage.LinkedStorageStackData;

import java.util.LinkedHashSet;
import java.util.Optional;
import java.util.Set;

public class SmithingBackpackUpgradeRecipe extends SmithingTransformRecipe implements IWrapperRecipe<SmithingTransformRecipe> {
	public static final Set<ResourceLocation> REGISTERED_RECIPES = new LinkedHashSet<>();
	private final SmithingTransformRecipe compose;

	public SmithingBackpackUpgradeRecipe(SmithingTransformRecipe compose) {
		super(compose.getId(), compose.template, compose.base, compose.addition, compose.result);
		this.compose = compose;
		REGISTERED_RECIPES.add(compose.getId());
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public boolean matches(Container inv, Level level) {
		return super.matches(inv, level) && getBackpack(inv).map(backpack -> BackpackUpgradeRecipe.canUpgrade(backpack, level)).orElse(false);
	}

	@Override
	public ItemStack assemble(Container inv, RegistryAccess registryAccess) {
		ItemStack upgradedBackpack = result.copy();
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			getBackpack(inv).flatMap(backpack -> Optional.ofNullable(backpack.getTag())).ifPresent(tag -> upgradedBackpack.setTag(tag.copy()));
			BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
			if (LinkedStorageStackData.getEndpoint(upgradedBackpack) != null) {
				upgradedBackpack.getOrCreateTag().putInt("inventorySlots", backpackItem.getNumberOfSlots());
				upgradedBackpack.getOrCreateTag().putInt("upgradeSlots", backpackItem.getNumberOfUpgradeSlots());
			} else {
				upgradedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
						.ifPresent(wrapper -> wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots()));
			}
		}
		return upgradedBackpack;
	}

	private Optional<ItemStack> getBackpack(Container inv) {
		ItemStack slotStack = inv.getItem(1);
		if (slotStack.getItem() instanceof BackpackItem) {
			return Optional.of(slotStack);
		}
		return Optional.empty();
	}

	@Override
	public RecipeSerializer<?> getSerializer() {
		return ModItems.SMITHING_BACKPACK_UPGRADE_RECIPE_SERIALIZER.get();
	}

	@Override
	public SmithingTransformRecipe getCompose() {
		return compose;
	}

	public Ingredient getTemplateIngredient() {
		return compose.template;
	}

	public Ingredient getBaseIngredient() {
		return compose.base;
	}

	public Ingredient getAdditionIngredient() {
		return compose.addition;
	}

	public static class Serializer extends RecipeWrapperSerializer<SmithingTransformRecipe, SmithingBackpackUpgradeRecipe> {
		public Serializer() {
			super(SmithingBackpackUpgradeRecipe::new, RecipeSerializer.SMITHING_TRANSFORM);
		}
	}
}
