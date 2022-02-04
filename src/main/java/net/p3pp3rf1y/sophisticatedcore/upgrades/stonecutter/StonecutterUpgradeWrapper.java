package net.p3pp3rf1y.sophisticatedcore.upgrades.stonecutter;

import net.minecraft.nbt.StringTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.function.Consumer;

public class StonecutterUpgradeWrapper extends UpgradeWrapperBase<StonecutterUpgradeWrapper, StonecutterUpgradeItem> {
	private static final String RECIPE_ID_TAG = "recipeId";
	private final IItemHandlerModifiable inputInventory;

	protected StonecutterUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);

		inputInventory = new ItemStackHandler(1) {
			@Override
			protected void onContentsChanged(int slot) {
				super.onContentsChanged(slot);
				if (slot == 0) {
					upgrade.addTagElement("input", getStackInSlot(0).serializeNBT());
				}
				save();
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
		upgrade.addTagElement(RECIPE_ID_TAG, StringTag.valueOf(recipeId.toString()));
		save();
	}

	public Optional<ResourceLocation> getRecipeId() {
		return NBTHelper.getString(upgrade, RECIPE_ID_TAG).map(ResourceLocation::new);
	}

	@Override
	public boolean canBeDisabled() {
		return false;
	}

	public boolean shouldShiftClickIntoStorage() {
		return NBTHelper.getBoolean(upgrade, "shiftClickIntoStorage").orElse(true);
	}

	public void setShiftClickIntoStorage(boolean shiftClickIntoStorage) {
		NBTHelper.setBoolean(upgrade, "shiftClickIntoStorage", shiftClickIntoStorage);
		save();
	}
}
