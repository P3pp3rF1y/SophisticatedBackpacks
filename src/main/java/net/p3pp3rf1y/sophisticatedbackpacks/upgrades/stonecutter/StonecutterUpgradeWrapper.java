package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.stonecutter;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.StringNBT;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.function.Consumer;

public class StonecutterUpgradeWrapper extends UpgradeWrapperBase<StonecutterUpgradeWrapper, StonecutterUpgradeItem> {
	private static final String RECIPE_ID_TAG = "recipeId";
	private final IItemHandlerModifiable inputInventory;

	protected StonecutterUpgradeWrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(backpackWrapper, upgrade, upgradeSaveHandler);

		inputInventory = new ItemStackHandler(1) {
			@Override
			protected void onContentsChanged(int slot) {
				super.onContentsChanged(slot);
				if (slot == 0) {
					upgrade.addTagElement("input", getStackInSlot(0).serializeNBT());
				}
			}
		};
		NBTHelper.getCompound(upgrade, "input").ifPresent(tag -> inputInventory.setStackInSlot(0, ItemStack.of(tag)));
	}

	public IItemHandlerModifiable getInputInventory() {
		return inputInventory;
	}

	public void setRecipeId(@Nullable ResourceLocation recipeId) {
		if (recipeId == null) {
			NBTHelper.removeTag(upgrade, RECIPE_ID_TAG);
			return;
		}
		upgrade.addTagElement(RECIPE_ID_TAG, StringNBT.valueOf(recipeId.toString()));
		save();
	}

	public Optional<ResourceLocation> getRecipeId() {
		return NBTHelper.getString(upgrade, RECIPE_ID_TAG).map(ResourceLocation::new);
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}

	public boolean shouldShiftClickIntoBackpack() {
		return NBTHelper.getBoolean(upgrade, "shiftClickIntoBackpack").orElse(true);
	}

	public void setShiftClickIntoBackpack(boolean shiftClickIntoBackpack) {
		NBTHelper.setBoolean(upgrade, "shiftClickIntoBackpack", shiftClickIntoBackpack);
		save();
	}
}
