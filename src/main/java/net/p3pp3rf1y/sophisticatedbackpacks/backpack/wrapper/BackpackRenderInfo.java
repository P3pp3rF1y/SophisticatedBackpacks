package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import com.google.common.collect.ImmutableMap;
import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class BackpackRenderInfo {
	private static final String RENDER_INFO_TAG = "renderInfo";
	private static final String TANKS_TAG = "tanks";
	private static final String UPGRADES_TAG = "upgrades";
	private static final String BATTERY_TAG = "battery";
	private static final String TANK_POSITION_TAG = "position";
	private static final String TANK_INFO_TAG = "info";
	private static final String ITEM_DISPLAY_TAG = "itemDisplay";

	private static final Map<String, UpgradeRenderDataType<?>> RENDER_DATA_TYPES;

	static {
		ImmutableMap.Builder<String, UpgradeRenderDataType<?>> builder = new ImmutableMap.Builder<>();
		builder.put(SmeltingUpgradeRenderData.TYPE.getName(), SmeltingUpgradeRenderData.TYPE);
		builder.put(JukeboxUpgradeRenderData.TYPE.getName(), JukeboxUpgradeRenderData.TYPE);
		RENDER_DATA_TYPES = builder.build();
	}

	private final ItemStack backpack;
	private final Supplier<Runnable> getBackpackSaveHandler;
	private final Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = new LinkedHashMap<>();
	@Nullable
	private IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;

	private final Map<UpgradeRenderDataType<?>, IUpgradeRenderData> upgradeData = new HashMap<>();
	private ItemDisplayRenderInfo itemDisplayRenderInfo = new ItemDisplayRenderInfo();

	public BackpackRenderInfo(ItemStack backpack, Supplier<Runnable> getBackpackSaveHandler) {
		this.backpack = backpack;
		this.getBackpackSaveHandler = getBackpackSaveHandler;
		deserialize();
	}

	public <T extends IUpgradeRenderData> Optional<T> getUpgradeRenderData(UpgradeRenderDataType<T> upgradeRenderDataType) {
		if (!upgradeData.containsKey(upgradeRenderDataType)) {
			return Optional.empty();
		}
		return upgradeRenderDataType.cast(upgradeData.get(upgradeRenderDataType));
	}

	public <T extends IUpgradeRenderData> void setUpgradeRenderData(UpgradeRenderDataType<T> upgradeRenderDataType, T renderData) {
		upgradeData.put(upgradeRenderDataType, renderData);
		serializeUpgradeData(upgrades -> upgrades.put(upgradeRenderDataType.getName(), renderData.serializeNBT()));
		save();
	}

	private void serializeUpgradeData(Consumer<CompoundNBT> modifyUpgradesTag) {
		CompoundNBT renderInfo = getRenderInfoTag();
		CompoundNBT upgrades = renderInfo.getCompound(UPGRADES_TAG);
		modifyUpgradesTag.accept(upgrades);
		renderInfo.put(UPGRADES_TAG, upgrades);
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
	}

	private void deserialize() {
		deserializeTanks();
		deserializeBattery();
		deserializeUpgrades();
		deserializeItemDisplay();
	}

	private void deserializeItemDisplay() {
		itemDisplayRenderInfo = ItemDisplayRenderInfo.deserialize(getRenderInfoTag().getCompound(ITEM_DISPLAY_TAG));
	}

	private void deserializeUpgrades() {
		CompoundNBT renderInfo = getRenderInfoTag();
		CompoundNBT upgrades = renderInfo.getCompound(UPGRADES_TAG);
		upgrades.getAllKeys().forEach(key -> {
			if (RENDER_DATA_TYPES.containsKey(key)) {
				UpgradeRenderDataType<?> upgradeRenderDataType = RENDER_DATA_TYPES.get(key);
				upgradeData.put(upgradeRenderDataType, upgradeRenderDataType.deserialize(upgrades.getCompound(key)));
			}
		});
	}

	private void save() {
		getBackpackSaveHandler.get().run();
	}

	public void deserializeFrom(CompoundNBT renderInfoNbt) {
		resetUpgradeInfo();
		backpack.addTagElement(RENDER_INFO_TAG, renderInfoNbt);
		deserialize();
	}

	public CompoundNBT getNbt() {
		return getRenderInfoTag();
	}

	public void resetUpgradeInfo() {
		tankRenderInfos.clear();
		batteryRenderInfo = null;
		getRenderInfoTag().remove(TANKS_TAG);
		getRenderInfoTag().remove(BATTERY_TAG);
		save();
	}

	public void setTankRenderInfo(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		tankRenderInfos.put(tankPosition, tankRenderInfo);
		serializeTank(tankPosition, tankRenderInfo);
		save();
	}

	private void deserializeTanks() {
		CompoundNBT renderInfo = getRenderInfoTag();
		ListNBT tanks = renderInfo.getList(TANKS_TAG, Constants.NBT.TAG_COMPOUND);
		for (int i = 0; i < tanks.size(); i++) {
			CompoundNBT tank = tanks.getCompound(i);
			tankRenderInfos.put(TankPosition.valueOf(tank.getString(TANK_POSITION_TAG).toUpperCase(Locale.ENGLISH)), IRenderedTankUpgrade.TankRenderInfo.deserialize(tank.getCompound(TANK_INFO_TAG)));
		}
	}

	private void deserializeBattery() {
		batteryRenderInfo = NBTHelper.getCompound(getRenderInfoTag(), BATTERY_TAG).map(IRenderedBatteryUpgrade.BatteryRenderInfo::deserialize).orElse(null);
	}

	private void serializeTank(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		CompoundNBT tankInfo = tankRenderInfo.serialize();

		CompoundNBT renderInfo = getRenderInfoTag();
		ListNBT tanks = renderInfo.getList(TANKS_TAG, Constants.NBT.TAG_COMPOUND);

		boolean infoSet = false;
		for (int i = 0; i < tanks.size(); i++) {
			CompoundNBT tank = tanks.getCompound(i);
			if (tank.getString(TANK_POSITION_TAG).equals(tankPosition.getSerializedName())) {
				tank.put(TANK_INFO_TAG, tankInfo);
				infoSet = true;
			}
		}
		if (!infoSet) {
			CompoundNBT tankPositionInfo = new CompoundNBT();
			tankPositionInfo.putString(TANK_POSITION_TAG, tankPosition.getSerializedName());
			tankPositionInfo.put(TANK_INFO_TAG, tankInfo);
			tanks.add(tankPositionInfo);
			renderInfo.put(TANKS_TAG, tanks);
		}

		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
	}

	public Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> getTankRenderInfos() {
		return tankRenderInfos;
	}

	public ItemDisplayRenderInfo getItemDisplayRenderInfo() {
		return itemDisplayRenderInfo;
	}

	public void setItemDisplayRenderInfo(ItemStack item, int rotation) {
		itemDisplayRenderInfo.setItem(item);
		itemDisplayRenderInfo.setRotation(rotation);
		CompoundNBT renderInfo = getRenderInfoTag();
		renderInfo.put(ITEM_DISPLAY_TAG, itemDisplayRenderInfo.serialize());
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
		save();
	}

	public Optional<IRenderedBatteryUpgrade.BatteryRenderInfo> getBatteryRenderInfo() {
		return Optional.ofNullable(batteryRenderInfo);
	}

	public void setBatteryRenderInfo(IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo) {
		this.batteryRenderInfo = batteryRenderInfo;
		CompoundNBT batteryInfo = batteryRenderInfo.serialize();
		CompoundNBT renderInfo = getRenderInfoTag();
		renderInfo.put(BATTERY_TAG, batteryInfo);
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
		save();
	}

	@Nonnull
	private CompoundNBT getRenderInfoTag() {
		return NBTHelper.getCompound(backpack, RENDER_INFO_TAG).orElse(new CompoundNBT());
	}

	public Map<UpgradeRenderDataType<?>, IUpgradeRenderData> getUpgradeRenderData() {
		return upgradeData;
	}

	public void removeUpgradeRenderData(UpgradeRenderDataType<?> type) {
		upgradeData.remove(type);
		serializeUpgradeData(upgrades -> upgrades.remove(type.getName()));
		save();
	}

	public static class ItemDisplayRenderInfo {
		private static final String ITEM_TAG = "item";
		private static final String ROTATION_TAG = "rotation";
		private ItemStack item;
		private int rotation;

		private ItemDisplayRenderInfo(ItemStack item, int rotation) {
			this.item = item;
			this.rotation = rotation;
		}

		public ItemDisplayRenderInfo() {
			this(ItemStack.EMPTY, 0);
		}

		public CompoundNBT serialize() {
			CompoundNBT ret = new CompoundNBT();
			if (!item.isEmpty()) {
				ret.put(ITEM_TAG, item.serializeNBT());
				ret.putInt(ROTATION_TAG, rotation);
			}
			return ret;
		}

		private void setItem(ItemStack item) {
			this.item = item;
		}

		public static ItemDisplayRenderInfo deserialize(CompoundNBT tag) {
			if (tag.contains(ITEM_TAG)) {
				return new ItemDisplayRenderInfo(ItemStack.of(tag.getCompound(ITEM_TAG)), tag.getInt(ROTATION_TAG));
			}
			return new ItemDisplayRenderInfo();
		}

		private void setRotation(int rot) {
			rotation = rot;
		}

		public int getRotation() {
			return rotation;
		}

		public ItemStack getItem() {
			return item;
		}
	}
}
