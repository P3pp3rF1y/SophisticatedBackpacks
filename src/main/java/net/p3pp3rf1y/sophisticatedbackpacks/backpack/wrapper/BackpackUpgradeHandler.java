package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackUpgradeItem;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeAccessModifier;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.util.InventoryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;

public class BackpackUpgradeHandler extends ItemStackHandler {
	private static final String UPGRADE_INVENTORY_TAG = "upgradeInventory";
	private final ItemStack backpack;
	private final IBackpackWrapper backpackWrapper;
	private final Consumer<ItemStack> backpackSaveHandler;
	private final Runnable onInvalidateUpgradeCaches;
	private final Map<Integer, IUpgradeWrapper> slotWrappers = new HashMap<>();
	private final Map<UpgradeType<? extends IUpgradeWrapper>, List<? extends IUpgradeWrapper>> typeWrappers = new HashMap<>();
	private boolean justSavingNbtChange = false;
	private boolean wrappersInitialized = false;
	private IUpgradeWrapperAccessor wrapperAccessor = null;

	public BackpackUpgradeHandler(ItemStack backpack, IBackpackWrapper backpackWrapper, Consumer<ItemStack> backpackSaveHandler, Runnable onInvalidateUpgradeCaches) {
		super(getNumberOfUpgradeSlots(backpack));
		this.backpack = backpack;
		this.backpackWrapper = backpackWrapper;
		this.backpackSaveHandler = backpackSaveHandler;
		this.onInvalidateUpgradeCaches = onInvalidateUpgradeCaches;
		NBTHelper.getCompound(backpack, UPGRADE_INVENTORY_TAG).ifPresent(this::deserializeNBT);
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		backpack.setTagInfo(UPGRADE_INVENTORY_TAG, serializeNBT());
		backpackSaveHandler.accept(backpack);
		if (!justSavingNbtChange) {
			wrappersInitialized = false;
			onInvalidateUpgradeCaches.run();
		}
	}

	private void initializeWrappers() {
		if (wrappersInitialized) {
			return;
		}
		wrappersInitialized = true;
		slotWrappers.clear();
		typeWrappers.clear();

		InventoryHelper.iterate(this, (slot, upgrade) -> {
			if (upgrade.isEmpty() || !(upgrade.getItem() instanceof IBackpackUpgradeItem<?>)) {
				return;
			}
			UpgradeType<?> type = ((IBackpackUpgradeItem<?>) upgrade.getItem()).getType();
			IUpgradeWrapper wrapper = type.create(backpackWrapper, upgrade, upgradeStack -> {
				justSavingNbtChange = true;
				setStackInSlot(slot, upgradeStack);
				justSavingNbtChange = false;
			});
			slotWrappers.put(slot, wrapper);
			addTypeWrapper(type, wrapper);
		});

		initializeUpgradeAccessor();
	}

	private void initializeUpgradeAccessor() {
		IUpgradeWrapperAccessor accessor = new Accessor(this);
		for (IUpgradeAccessModifier upgrade : getListOfWrappersThatImplement(IUpgradeAccessModifier.class)) {
			accessor = upgrade.wrapAccessor(accessor);
		}
		wrapperAccessor = accessor;
	}

	private <T extends IUpgradeWrapper> void addTypeWrapper(UpgradeType<?> type, T wrapper) {
		//noinspection unchecked
		((List<T>) typeWrappers.computeIfAbsent(type, t -> new ArrayList<>())).add(wrapper);
	}

	public <T extends IUpgradeWrapper> List<T> getTypeWrappers(UpgradeType<T> type) {
		initializeWrappers();
		//noinspection unchecked
		return (List<T>) typeWrappers.getOrDefault(type, Collections.emptyList());
	}

	public <T extends IUpgradeWrapper> boolean hasUpgrade(UpgradeType<T> type) {
		return !getTypeWrappers(type).isEmpty();
	}

	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		initializeWrappers();
		return wrapperAccessor.getWrappersThatImplement(upgradeClass);
	}

	public <T> List<T> getWrappersThatImplementFromMainBackpack(Class<T> upgradeClass) {
		initializeWrappers();
		return wrapperAccessor.getWrappersThatImplementFromMainBackpack(upgradeClass);
	}

	public <T> List<T> getListOfWrappersThatImplement(Class<T> uc) {
		List<T> ret = new ArrayList<>();
		for (IUpgradeWrapper wrapper : slotWrappers.values()) {
			if (uc.isInstance(wrapper)) {
				//noinspection unchecked
				ret.add((T) wrapper);
			}
		}
		return ret;
	}

	public Map<Integer, IUpgradeWrapper> getSlotWrappers() {
		initializeWrappers();
		return slotWrappers;
	}

	@Override
	public boolean isItemValid(int slot, ItemStack stack) {
		return stack.getItem() instanceof IBackpackUpgradeItem && ((IBackpackUpgradeItem<?>) stack.getItem()).canAddUpgradeTo(backpackWrapper);
	}

	private static int getNumberOfUpgradeSlots(ItemStack backpack) {
		return ((BackpackItem) backpack.getItem()).getNumberOfUpgradeSlots();
	}

	public void copyTo(BackpackUpgradeHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	private static class Accessor implements IUpgradeWrapperAccessor {
		private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();

		private final BackpackUpgradeHandler upgradeHandler;

		public Accessor(BackpackUpgradeHandler upgradeHandler) {
			this.upgradeHandler = upgradeHandler;
		}

		@Override
		public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
			//noinspection unchecked
			return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, upgradeHandler::getListOfWrappersThatImplement);
		}

		@Override
		public <T> List<T> getWrappersThatImplementFromMainBackpack(Class<T> upgradeClass) {
			//noinspection unchecked
			return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, upgradeHandler::getListOfWrappersThatImplement);
		}

		@Override
		public void clearCache() {
			interfaceWrappers.clear();
		}
	}
}

