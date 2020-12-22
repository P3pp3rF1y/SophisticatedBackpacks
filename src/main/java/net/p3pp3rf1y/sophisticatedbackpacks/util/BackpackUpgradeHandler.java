package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.IItemHandlerModifiable;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.items.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.Direction;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.filter.FilterUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.inception.InceptionUpgradeItem;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class BackpackUpgradeHandler extends ItemStackHandler {
	private static final String UPGRADE_INVENTORY_TAG = "upgradeInventory";
	private final ItemStack backpack;
	private final Consumer<ItemStack> backpackSaveHandler;
	private final Map<Integer, IUpgradeWrapper> slotWrappers = new HashMap<>();
	private final Map<UpgradeType<? extends IUpgradeWrapper>, List<? extends IUpgradeWrapper>> typeWrappers = new HashMap<>();
	private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();
	private boolean justSavingNbtChange = false;
	private boolean wrappersInitialized = false;
	@Nullable
	private IItemHandlerModifiable filteredHandler = null;
	@Nullable
	private IItemHandlerModifiable inceptionInventoryHandler = null;

	public BackpackUpgradeHandler(ItemStack backpack, Consumer<ItemStack> backpackSaveHandler) {
		super(getNumberOfUpgradeSlots(backpack));
		this.backpack = backpack;
		this.backpackSaveHandler = backpackSaveHandler;
		NBTHelper.getCompound(backpack, UPGRADE_INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(UPGRADE_INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
		if (!justSavingNbtChange) {
			wrappersInitialized = false;
			filteredHandler = null;
			inceptionInventoryHandler = null;
		}
	}

	private void initializeWrappers() {
		if (wrappersInitialized) {
			return;
		}
		wrappersInitialized = true;
		slotWrappers.clear();
		typeWrappers.clear();
		interfaceWrappers.clear();

		InventoryHelper.iterate(this, (slot, upgrade) -> {
			if (upgrade.isEmpty() || !(upgrade.getItem() instanceof IBackpackUpgradeItem<?>)) {
				return;
			}
			UpgradeType<?> type = ((IBackpackUpgradeItem<?>) upgrade.getItem()).getType();
			IUpgradeWrapper wrapper = type.create(upgrade, upgradeStack -> {
				justSavingNbtChange = true;
				setStackInSlot(slot, upgradeStack);
				justSavingNbtChange = false;
			});
			slotWrappers.put(slot, wrapper);
			addTypeWrapper(type, wrapper);
		});
	}

	private <T extends IUpgradeWrapper> void addTypeWrapper(UpgradeType<?> type, T wrapper) {
		if (!typeWrappers.containsKey(type)) {
			typeWrappers.put(type, new ArrayList<>());
		}
		//noinspection unchecked
		((List<T>) typeWrappers.get(type)).add(wrapper);
	}

	public <T extends IUpgradeWrapper> List<T> getTypeWrappers(UpgradeType<T> type) {
		initializeWrappers();
		//noinspection unchecked
		return (List<T>) typeWrappers.getOrDefault(type, Collections.emptyList());
	}

	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		initializeWrappers();
		if (!interfaceWrappers.containsKey(upgradeClass)) {
			List<T> ret = new ArrayList<>();
			for (IUpgradeWrapper wrapper : slotWrappers.values()) {
				if (upgradeClass.isInstance(wrapper)) {
					//noinspection unchecked
					ret.add((T) wrapper);
				}
			}
			interfaceWrappers.put(upgradeClass, ret);
		}
		//noinspection unchecked
		return (List<T>) interfaceWrappers.get(upgradeClass);
	}

	public Map<Integer, IUpgradeWrapper> getSlotWrappers() {
		initializeWrappers();
		return slotWrappers;
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return stack.getItem() instanceof IBackpackUpgradeItem;
	}

	private static int getNumberOfUpgradeSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfUpgradeSlots();
	}

	public void copyTo(BackpackUpgradeHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public IItemHandlerModifiable getFilteredHandler(IItemHandlerModifiable inventoryHandler) {
		initializeFilteredHandler(inventoryHandler);
		//noinspection ConstantConditions - can't be null as it gets initialized in the line above
		return filteredHandler;
	}

	private void initializeFilteredHandler(IItemHandlerModifiable inventoryHandler) {
		initializeWrappers();
		if (filteredHandler != null) {
			return;
		}

		List<FilterLogic> inputFilters = new ArrayList<>();
		List<FilterLogic> outputFilters = new ArrayList<>();

		addFilters(inputFilters, outputFilters);

		if (inputFilters.isEmpty() && outputFilters.isEmpty()) {
			filteredHandler = inventoryHandler;
		} else {
			filteredHandler = new FilteredItemHandler.Modifiable(inventoryHandler, inputFilters, outputFilters);
		}
	}

	private void addFilters(List<FilterLogic> inputFilters, List<FilterLogic> outputFilters) {
		List<FilterUpgradeWrapper> filterWrappers = getTypeWrappers(FilterUpgradeItem.TYPE);

		for (FilterUpgradeWrapper wrapper : filterWrappers) {
			Direction dir = wrapper.getDirection();

			FilterLogic filterLogic = wrapper.getFilterLogic();
			switch (dir) {
				case BOTH:
					inputFilters.add(filterLogic);
					outputFilters.add(filterLogic);
					break;
				case INPUT:
					inputFilters.add(filterLogic);
					break;
				case OUTPUT:
					outputFilters.add(filterLogic);
					break;
			}
		}
	}

	public boolean hasInceptionUpgrade() {
		return !getTypeWrappers(InceptionUpgradeItem.TYPE).isEmpty();
	}

	public IItemHandlerModifiable getInceptionInventoryHandler(BackpackInventoryHandler inventoryHandler) {
		if (inceptionInventoryHandler == null) {
			if (hasInceptionUpgrade()) {
				inceptionInventoryHandler = new InceptionInventoryHandler(backpack, inventoryHandler);
				inventoryHandler.setMainInventoryHandlerWrappedByInception(true);
			} else {
				inceptionInventoryHandler = inventoryHandler;
				inventoryHandler.setMainInventoryHandlerWrappedByInception(false);
			}
		}
		return inceptionInventoryHandler;
	}
}

