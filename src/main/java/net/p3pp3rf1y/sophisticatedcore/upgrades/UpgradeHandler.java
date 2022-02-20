package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.core.NonNullList;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.items.ItemStackHandler;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.renderdata.TankPosition;
import net.p3pp3rf1y.sophisticatedcore.util.InventoryHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

public class UpgradeHandler extends ItemStackHandler {
	public static final String UPGRADE_INVENTORY_TAG = "upgradeInventory";
	private final IStorageWrapper storageWrapper;
	private final Runnable contentsSaveHandler;
	private final Runnable onInvalidateUpgradeCaches;
	private final CompoundTag contentsNbt;
	@Nullable
	private Runnable refreshCallBack = null;
	private final Map<Integer, IUpgradeWrapper> slotWrappers = new LinkedHashMap<>();
	private final Map<UpgradeType<? extends IUpgradeWrapper>, List<? extends IUpgradeWrapper>> typeWrappers = new HashMap<>();
	private boolean justSavingNbtChange = false;
	private boolean wrappersInitialized = false;
	private boolean typeWrappersInitialized = false;
	@Nullable
	private IUpgradeWrapperAccessor wrapperAccessor = null;
	private boolean persistent = true;

	public UpgradeHandler(int numberOfUpgradeSlots, IStorageWrapper storageWrapper, CompoundTag contentsNbt, Runnable contentsSaveHandler, Runnable onInvalidateUpgradeCaches) {
		super(numberOfUpgradeSlots);
		this.contentsNbt = contentsNbt;
		this.storageWrapper = storageWrapper;
		this.contentsSaveHandler = contentsSaveHandler;
		this.onInvalidateUpgradeCaches = onInvalidateUpgradeCaches;
		deserializeNBT(contentsNbt.getCompound(UPGRADE_INVENTORY_TAG));
	}

	@Override
	public boolean isItemValid(int slot, @Nonnull ItemStack stack) {
		return stack.isEmpty() || stack.getItem() instanceof IUpgradeItem;
	}

	@Override
	protected void onContentsChanged(int slot) {
		super.onContentsChanged(slot);
		if (persistent) {
			saveInventory();
			contentsSaveHandler.run();
		}
		if (!justSavingNbtChange) {
			refreshUpgradeWrappers();
		}
	}

	@Override
	public void setSize(int size) {
		super.setSize(stacks.size());
	}

	public void saveInventory() {
		contentsNbt.put(UPGRADE_INVENTORY_TAG, serializeNBT());
	}

	public void setPersistent(boolean persistent) {
		this.persistent = persistent;
	}

	public void setRefreshCallBack(Runnable refreshCallBack) {
		this.refreshCallBack = refreshCallBack;
	}

	public void removeRefreshCallback() {
		refreshCallBack = null;
	}

	private void initializeWrappers() {
		if (wrappersInitialized) {
			return;
		}
		wrappersInitialized = true;
		slotWrappers.clear();
		typeWrappers.clear();
		if (wrapperAccessor != null) {
			wrapperAccessor.clearCache();
		}

		InventoryHelper.iterate(this, (slot, upgrade) -> {
			if (upgrade.isEmpty() || !(upgrade.getItem() instanceof IUpgradeItem<?>)) {
				return;
			}
			UpgradeType<?> type = ((IUpgradeItem<?>) upgrade.getItem()).getType();
			IUpgradeWrapper wrapper = type.create(storageWrapper, upgrade, upgradeStack -> {
				justSavingNbtChange = true;
				setStackInSlot(slot, upgradeStack);
				justSavingNbtChange = false;
			});
			slotWrappers.put(slot, wrapper);
		});

		initRenderInfoCallbacks(false);
	}

	@Override
	public ItemStack extractItem(int slot, int amount, boolean simulate) {
		ItemStack slotStack = getStackInSlot(slot);
		if (persistent && !slotStack.isEmpty() && amount == 1) {
			Map<Integer, IUpgradeWrapper> wrappers = getSlotWrappers();
			if (wrappers.containsKey(slot)) {
				wrappers.get(slot).onBeforeRemoved();
			}
		}
		return super.extractItem(slot, amount, simulate);
	}

	private void initializeTypeWrappers() {
		if (typeWrappersInitialized) {
			return;
		}
		initializeWrappers();
		typeWrappersInitialized = true;

		slotWrappers.values().forEach(wrapper -> {
			if (wrapper.getUpgradeStack().getItem() instanceof IUpgradeItem<?> upgradeItem) {
				UpgradeType<?> type = upgradeItem.getType();
				if (wrapper.isEnabled()) {
					addTypeWrapper(type, wrapper);
				}
			}
		});

		initRenderInfoCallbacks(false);
	}

	private <T extends IUpgradeWrapper> void addTypeWrapper(UpgradeType<?> type, T wrapper) {
		//noinspection unchecked
		((List<T>) typeWrappers.computeIfAbsent(type, t -> new ArrayList<>())).add(wrapper);
	}

	public <T extends IUpgradeWrapper> List<T> getTypeWrappers(UpgradeType<T> type) {
		initializeTypeWrappers();
		//noinspection unchecked
		return (List<T>) typeWrappers.getOrDefault(type, Collections.emptyList());
	}

	public <T extends IUpgradeWrapper> boolean hasUpgrade(UpgradeType<T> type) {
		return !getTypeWrappers(type).isEmpty();
	}

	public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
		initializeWrappers();
		return getWrapperAccessor().getWrappersThatImplement(upgradeClass);
	}

	private IUpgradeWrapperAccessor getWrapperAccessor() {
		if (wrapperAccessor == null) {
			IUpgradeWrapperAccessor accessor = new Accessor(this);
			for (IUpgradeAccessModifier upgrade : getListOfWrappersThatImplement(IUpgradeAccessModifier.class)) {
				accessor = upgrade.wrapAccessor(accessor);
			}
			wrapperAccessor = accessor;
		}
		return wrapperAccessor;
	}

	public <T> List<T> getWrappersThatImplementFromMainStorage(Class<T> upgradeClass) {
		initializeWrappers();
		return getWrapperAccessor().getWrappersThatImplementFromMainStorage(upgradeClass);
	}

	public <T> List<T> getListOfWrappersThatImplement(Class<T> uc) {
		List<T> ret = new ArrayList<>();
		for (IUpgradeWrapper wrapper : slotWrappers.values()) {
			if (wrapper.isEnabled() && uc.isInstance(wrapper)) {
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

	public void copyTo(UpgradeHandler otherHandler) {
		InventoryHelper.copyTo(this, otherHandler);
	}

	public void refreshWrappersThatImplementAndTypeWrappers() {
		typeWrappersInitialized = false;
		if (wrapperAccessor != null) {
			wrapperAccessor.clearCache();
		}
		if (refreshCallBack != null) {
			refreshCallBack.run();
		}
	}

	public void refreshUpgradeWrappers() {
		wrappersInitialized = false;
		typeWrappersInitialized = false;
		if (wrapperAccessor != null) {
			wrapperAccessor.onBeforeDeconstruct();
			wrapperAccessor = null;
		}
		if (refreshCallBack != null) {
			refreshCallBack.run();
		}
		onInvalidateUpgradeCaches.run();

		initRenderInfoCallbacks(true);
	}

	private void initRenderInfoCallbacks(boolean forceUpdateRenderInfo) {
		RenderInfo renderInfo = storageWrapper.getRenderInfo();
		if (forceUpdateRenderInfo) {
			renderInfo.resetUpgradeInfo();
		}

		initTankRenderInfoCallbacks(forceUpdateRenderInfo, renderInfo);
		initBatteryRenderInfoCallbacks(forceUpdateRenderInfo, renderInfo);
	}

	private void initBatteryRenderInfoCallbacks(boolean forceUpdateRenderInfo, RenderInfo renderInfo) {
		getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof IRenderedBatteryUpgrade batteryWrapper) {
				batteryWrapper.setBatteryRenderInfoUpdateCallback(renderInfo::setBatteryRenderInfo);
				if (forceUpdateRenderInfo) {
					batteryWrapper.forceUpdateBatteryRenderInfo();
				}
			}
		});
	}

	private void initTankRenderInfoCallbacks(boolean forceUpdateRenderInfo, RenderInfo renderInfo) {
		AtomicBoolean singleTankRight = new AtomicBoolean(false);
		List<IRenderedTankUpgrade> tankRenderWrappers = new ArrayList<>();
		int minRightSlot = getSlots() / 2;
		getSlotWrappers().forEach((slot, wrapper) -> {
			if (wrapper instanceof IRenderedTankUpgrade tankUpgrade) {
				tankRenderWrappers.add(tankUpgrade);
				if (slot >= minRightSlot) {
					singleTankRight.set(true);
				}
			}
		});

		TankPosition currentTankPos = tankRenderWrappers.size() == 1 && singleTankRight.get() ? TankPosition.RIGHT : TankPosition.LEFT;
		for (IRenderedTankUpgrade tankRenderWrapper : tankRenderWrappers) {
			TankPosition finalCurrentTankPos = currentTankPos;
			tankRenderWrapper.setTankRenderInfoUpdateCallback(tankRenderInfo -> renderInfo.setTankRenderInfo(finalCurrentTankPos, tankRenderInfo));
			if (forceUpdateRenderInfo) {
				tankRenderWrapper.forceUpdateTankRenderInfo();
			}
			currentTankPos = TankPosition.RIGHT;
		}
	}

	public void increaseSize(int diff) {
		NonNullList<ItemStack> previousStacks = stacks;
		stacks = NonNullList.withSize(previousStacks.size() + diff, ItemStack.EMPTY);
		for (int slot = 0; slot < previousStacks.size(); slot++) {
			stacks.set(slot, previousStacks.get(slot));
		}
		saveInventory();
	}

	private static class Accessor implements IUpgradeWrapperAccessor {
		private final Map<Class<?>, List<?>> interfaceWrappers = new HashMap<>();

		private final UpgradeHandler upgradeHandler;

		public Accessor(UpgradeHandler upgradeHandler) {
			this.upgradeHandler = upgradeHandler;
		}

		@Override
		public <T> List<T> getWrappersThatImplement(Class<T> upgradeClass) {
			//noinspection unchecked
			return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, upgradeHandler::getListOfWrappersThatImplement);
		}

		@Override
		public <T> List<T> getWrappersThatImplementFromMainStorage(Class<T> upgradeClass) {
			//noinspection unchecked
			return (List<T>) interfaceWrappers.computeIfAbsent(upgradeClass, upgradeHandler::getListOfWrappersThatImplement);
		}

		@Override
		public void clearCache() {
			interfaceWrappers.clear();
		}
	}
}

