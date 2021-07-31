package net.p3pp3rf1y.sophisticatedbackpacks.crafting;

import net.minecraft.inventory.IInventory;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipeSerializer;
import net.minecraft.item.crafting.SmithingRecipe;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.fml.common.thread.SidedThreadGroups;
import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;

import java.util.Objects;
import java.util.Optional;

public class SmithingBackpackUpgradeRecipe extends SmithingRecipe implements IWrapperRecipe<SmithingRecipe> {
	public static final Serializer SERIALIZER = new Serializer();
	private final SmithingRecipe compose;

	public SmithingBackpackUpgradeRecipe(SmithingRecipe compose) {
		super(compose.getId(), Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(SmithingRecipe.class, compose, "field_234837_a_")),
				Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(SmithingRecipe.class, compose, "field_234838_b_")), compose.getResultItem());
		this.compose = compose;
	}

	@Override
	public ItemStack assemble(IInventory inv) {
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
		return Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(SmithingRecipe.class, this, "field_234839_c_"));
	}

	private Optional<ItemStack> getBackpack(IInventory inv) {
		ItemStack slotStack = inv.getItem(0);
		if (slotStack.getItem() instanceof BackpackItem) {
			return Optional.of(slotStack);
		}
		return Optional.empty();
	}

	@Override
	public Serializer getSerializer() {
		return SERIALIZER;
	}

	@Override
	public SmithingRecipe getCompose() {
		return compose;
	}

	public static class Serializer extends RecipeWrapperSerializer<SmithingRecipe, SmithingBackpackUpgradeRecipe> {
		public Serializer() {
			super(SmithingBackpackUpgradeRecipe::new, IRecipeSerializer.SMITHING);
		}
	}
}
