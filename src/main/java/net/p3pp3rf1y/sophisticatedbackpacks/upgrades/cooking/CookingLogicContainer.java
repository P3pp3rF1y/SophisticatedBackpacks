package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class CookingLogicContainer<T extends AbstractCookingRecipe> {
	private final Supplier<CookingLogic<T>> supplyCoookingLogic;

	private final List<Slot> smeltingSlots = new ArrayList<>();

	public CookingLogicContainer(Supplier<CookingLogic<T>> supplyCoookingLogic, Consumer<Slot> addSlot) {
		this.supplyCoookingLogic = supplyCoookingLogic;

		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplyCoookingLogic.get().getCookingInventory(), CookingLogic.COOK_INPUT_SLOT, -100, -100));
		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplyCoookingLogic.get().getCookingInventory(), CookingLogic.FUEL_SLOT, -100, -100));
		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplyCoookingLogic.get().getCookingInventory(), CookingLogic.COOK_OUTPUT_SLOT, -100, -100) {
			@Override
			public boolean mayPlace(ItemStack stack) {
				return false; //needs to not allow player putting anything in
			}
		});
	}

	private void addSmeltingSlot(Consumer<Slot> addSlot, Slot slot) {
		addSlot.accept(slot);
		smeltingSlots.add(slot);
	}

	public int getBurnTimeTotal() {
		return supplyCoookingLogic.get().getBurnTimeTotal();
	}

	public long getBurnTimeFinish() {
		return supplyCoookingLogic.get().getBurnTimeFinish();
	}

	public long getCookTimeFinish() {
		return supplyCoookingLogic.get().getCookTimeFinish();
	}

	public int getCookTimeTotal() {
		return supplyCoookingLogic.get().getCookTimeTotal();
	}

	public boolean isCooking() {
		return supplyCoookingLogic.get().isCooking();
	}

	public boolean isBurning(World world) {
		return supplyCoookingLogic.get().isBurning(world);
	}

	public List<Slot> getCookingSlots() {
		return smeltingSlots;
	}
}
