package net.p3pp3rf1y.sophisticatedcore.settings.itemdisplay;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.inventory.InventoryHandler;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.settings.ISettingsCategory;
import net.p3pp3rf1y.sophisticatedcore.settings.ISlotColorCategory;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class ItemDisplaySettingsCategory implements ISettingsCategory, ISlotColorCategory {
	public static final String NAME = "item_display";
	private static final String SLOT_TAG = "slot";
	private static final String ROTATION_TAG = "rotation";
	private static final String COLOR_TAG = "color";
	private final Supplier<InventoryHandler> inventoryHandlerSupplier;
	private final Supplier<RenderInfo> renderInfoSupplier;
	private CompoundTag categoryNbt;
	private final Consumer<CompoundTag> saveNbt;
	private DyeColor color = DyeColor.RED;
	private int slotIndex = -1;
	private int rotation = 0;

	public ItemDisplaySettingsCategory(Supplier<InventoryHandler> inventoryHandlerSupplier, Supplier<RenderInfo> renderInfoSupplier, CompoundTag categoryNbt, Consumer<CompoundTag> saveNbt) {
		this.inventoryHandlerSupplier = inventoryHandlerSupplier;
		this.renderInfoSupplier = renderInfoSupplier;
		this.categoryNbt = categoryNbt;
		this.saveNbt = saveNbt;

		deserialize();
	}

	public void unselectSlot() {
		slotIndex = -1;
		categoryNbt.remove(SLOT_TAG);
		saveNbt.accept(categoryNbt);
		updateRenderInfo();
	}

	private void updateRenderInfo() {
		RenderInfo renderInfo = renderInfoSupplier.get();
		if (slotIndex >= 0) {
			ItemStack stackCopy = inventoryHandlerSupplier.get().getStackInSlot(slotIndex).copy();
			stackCopy.setCount(1);
			renderInfo.setItemDisplayRenderInfo(stackCopy, rotation);
		} else {
			renderInfo.setItemDisplayRenderInfo(ItemStack.EMPTY, 0);
		}
	}

	public void selectSlot(int slot) {
		slotIndex = slot;
		categoryNbt.putInt(SLOT_TAG, slot);
		saveNbt.accept(categoryNbt);
		updateRenderInfo();
	}

	public Optional<Integer> getSlot() {
		return slotIndex >= 0 ? Optional.of(slotIndex) : Optional.empty();
	}

	public int getRotation() {
		return rotation;
	}

	public void rotate(boolean clockwise) {
		rotation = (rotation + ((clockwise ? 1 : -1) * 45) + 360) % 360;
		categoryNbt.putInt(ROTATION_TAG, rotation);
		saveNbt.accept(categoryNbt);
		updateRenderInfo();
	}

	public void setColor(DyeColor color) {
		this.color = color;
		categoryNbt.putInt(COLOR_TAG, color.getId());
		saveNbt.accept(categoryNbt);
	}

	public DyeColor getColor() {
		return color;
	}

	@Override
	public void reloadFrom(CompoundTag categoryNbt) {
		this.categoryNbt = categoryNbt;
		deserialize();
	}

	private void deserialize() {
		slotIndex = NBTHelper.getInt(categoryNbt, SLOT_TAG).orElse(-1);
		rotation = NBTHelper.getInt(categoryNbt, ROTATION_TAG).orElse(0);
		color = NBTHelper.getInt(categoryNbt, COLOR_TAG).map(DyeColor::byId).orElse(DyeColor.RED);
	}

	public void itemChanged(int changedSlotIndex) {
		if (changedSlotIndex != slotIndex) {
			return;
		}
		updateRenderInfo();
	}

	@Override
	public Optional<Integer> getSlotColor(int slotNumber) {
		return slotIndex == slotNumber ? Optional.of(ColorHelper.getColor(color.getTextureDiffuseColors())) : Optional.empty();
	}
}
