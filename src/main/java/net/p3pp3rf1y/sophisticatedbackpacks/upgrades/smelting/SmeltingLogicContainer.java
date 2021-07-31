package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.inventory.container.Slot;
import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.SlotSuppliedHandler;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class SmeltingLogicContainer {
	private final Supplier<SmeltingLogic> supplySmeltingLogic;

	private final List<Slot> smeltingSlots = new ArrayList<>();

	public SmeltingLogicContainer(Supplier<SmeltingLogic> supplySmeltingLogic, Consumer<Slot> addSlot) {
		this.supplySmeltingLogic = supplySmeltingLogic;

		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplySmeltingLogic.get().getSmeltingInventory(), SmeltingLogic.COOK_INPUT_SLOT, -100, -100));
		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplySmeltingLogic.get().getSmeltingInventory(), SmeltingLogic.FUEL_SLOT, -100, -100));
		addSmeltingSlot(addSlot, new SlotSuppliedHandler(() -> supplySmeltingLogic.get().getSmeltingInventory(), SmeltingLogic.COOK_OUTPUT_SLOT, -100, -100) {
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
		return supplySmeltingLogic.get().getBurnTimeTotal();
	}

	public long getBurnTimeFinish() {
		return supplySmeltingLogic.get().getBurnTimeFinish();
	}

	public long getCookTimeFinish() {
		return supplySmeltingLogic.get().getCookTimeFinish();
	}

	public int getCookTimeTotal() {
		return supplySmeltingLogic.get().getCookTimeTotal();
	}

	public boolean isCooking() {
		return supplySmeltingLogic.get().isCooking();
	}

	public boolean isBurning(World world) {
		return supplySmeltingLogic.get().isBurning(world);
	}

	public List<Slot> getSmeltingSlots() {
		return smeltingSlots;
	}
}
