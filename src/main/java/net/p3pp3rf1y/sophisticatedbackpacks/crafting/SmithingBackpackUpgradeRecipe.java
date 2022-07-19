package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.Container;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.RecipeSerializer;
import net.minecraft.world.item.crafting.UpgradeRecipe;
import net.minecraftforge.fml.util.ObfuscationReflectionHelper;
import net.minecraftforge.fml.util.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.crafting.IWrapperRecipe;
import net.p3pp3rf1y.sophisticatedcore.crafting.RecipeWrapperSerializer;

import java.util.LinkedHashSet;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

public class SmithingBackpackUpgradeRecipe extends UpgradeRecipe implements IWrapperRecipe<UpgradeRecipe> {
	public static final Set<ResourceLocation> REGISTERED_RECIPES = new LinkedHashSet<>();
	private final UpgradeRecipe compose;

	public SmithingBackpackUpgradeRecipe(UpgradeRecipe compose) {
		super(compose.getId(), Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(UpgradeRecipe.class, compose, "f_44518_")),
				Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(UpgradeRecipe.class, compose, "f_44519_")), compose.getResultItem());
		this.compose = compose;
		REGISTERED_RECIPES.add(compose.getId());
	}

	@Override
	public boolean isSpecial() {
		return true;
	}

	@Override
	public ItemStack assemble(Container inv) {
		ItemStack upgradedBackpack = getCraftingResult().copy();
		if (Thread.currentThread().getThreadGroup() == SidedThreadGroups.SERVER) {
			getBackpack(inv).flatMap(backpack -> Optional.ofNullable(backpack.getTag())).ifPresent(tag -> upgradedBackpack.setTag(tag.copy()));
			upgradedBackpack.getCapability(CapabilityBackpackWrapper.getCapabilityInstance())
					.ifPresent(wrapper -> {
						BackpackItem backpackItem = ((BackpackItem) upgradedBackpack.getItem());
						wrapper.setSlotNumbers(backpackItem.getNumberOfSlots(), backpackItem.getNumberOfUpgradeSlots());
					});
		}
		return upgradedBackpack;
	}

	private ItemStack getCraftingResult() {
		return Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(UpgradeRecipe.class, this, "f_44520_"));
	}

	private Optional<ItemStack> getBackpack(Container inv) {
		ItemStack slotStack = inv.getItem(0);
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
	public UpgradeRecipe getCompose() {
		return compose;
	}

	public static class Serializer extends RecipeWrapperSerializer<UpgradeRecipe, SmithingBackpackUpgradeRecipe> {
		public Serializer() {
			super(SmithingBackpackUpgradeRecipe::new, RecipeSerializer.SMITHING);
		}
	}
}
