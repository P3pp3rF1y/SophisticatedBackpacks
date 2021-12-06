package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;
import net.minecraft.nbt.Tag;
import net.minecraft.world.item.ItemStack;
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

	private static final Map<String, UpgradeRenderDataType<?>> RENDER_DATA_TYPES;

	static {
		RENDER_DATA_TYPES = Map.of(
				SmeltingUpgradeRenderData.TYPE.getName(), SmeltingUpgradeRenderData.TYPE,
				JukeboxUpgradeRenderData.TYPE.getName(), JukeboxUpgradeRenderData.TYPE
		);
	}

	private final ItemStack backpack;
	private final Supplier<Runnable> getBackpackSaveHandler;
	private final Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = new LinkedHashMap<>();
	@Nullable
	private IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;

	private final Map<UpgradeRenderDataType<?>, IUpgradeRenderData> upgradeData = new HashMap<>();

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

	private void serializeUpgradeData(Consumer<CompoundTag> modifyUpgradesTag) {
		CompoundTag renderInfo = getRenderInfoTag();
		CompoundTag upgrades = renderInfo.getCompound(UPGRADES_TAG);
		modifyUpgradesTag.accept(upgrades);
		renderInfo.put(UPGRADES_TAG, upgrades);
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
	}

	private void deserialize() {
		deserializeTanks();
		deserializeBattery();
		deserializeUpgrades();
	}

	private void deserializeUpgrades() {
		CompoundTag renderInfo = getRenderInfoTag();
		CompoundTag upgrades = renderInfo.getCompound(UPGRADES_TAG);
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

	public void deserializeFrom(CompoundTag renderInfoNbt) {
		reset();
		backpack.addTagElement(RENDER_INFO_TAG, renderInfoNbt);
		deserialize();
	}

	public CompoundTag getNbt() {
		return getRenderInfoTag();
	}

	public void reset() {
		tankRenderInfos.clear();
		batteryRenderInfo = null;
		NBTHelper.removeTag(backpack, RENDER_INFO_TAG);
		save();
	}

	public void setTankRenderInfo(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		tankRenderInfos.put(tankPosition, tankRenderInfo);
		serializeTank(tankPosition, tankRenderInfo);
		save();
	}

	private void deserializeTanks() {
		CompoundTag renderInfo = getRenderInfoTag();
		ListTag tanks = renderInfo.getList(TANKS_TAG, Tag.TAG_COMPOUND);
		for (int i = 0; i < tanks.size(); i++) {
			CompoundTag tank = tanks.getCompound(i);
			tankRenderInfos.put(TankPosition.valueOf(tank.getString(TANK_POSITION_TAG).toUpperCase(Locale.ENGLISH)), IRenderedTankUpgrade.TankRenderInfo.deserialize(tank.getCompound(TANK_INFO_TAG)));
		}
	}

	private void deserializeBattery() {
		batteryRenderInfo = NBTHelper.getCompound(getRenderInfoTag(), BATTERY_TAG).map(IRenderedBatteryUpgrade.BatteryRenderInfo::deserialize).orElse(null);
	}

	private void serializeTank(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		CompoundTag tankInfo = tankRenderInfo.serialize();

		CompoundTag renderInfo = getRenderInfoTag();
		ListTag tanks = renderInfo.getList(TANKS_TAG, Tag.TAG_COMPOUND);

		boolean infoSet = false;
		for (int i = 0; i < tanks.size(); i++) {
			CompoundTag tank = tanks.getCompound(i);
			if (tank.getString(TANK_POSITION_TAG).equals(tankPosition.getSerializedName())) {
				tank.put(TANK_INFO_TAG, tankInfo);
				infoSet = true;
			}
		}
		if (!infoSet) {
			CompoundTag tankPositionInfo = new CompoundTag();
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

	public Optional<IRenderedBatteryUpgrade.BatteryRenderInfo> getBatteryRenderInfo() {
		return Optional.ofNullable(batteryRenderInfo);
	}

	public void setBatteryRenderInfo(IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo) {
		this.batteryRenderInfo = batteryRenderInfo;
		CompoundTag batteryInfo = batteryRenderInfo.serialize();
		CompoundTag renderInfo = getRenderInfoTag();
		renderInfo.put(BATTERY_TAG, batteryInfo);
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
		save();
	}

	@Nonnull
	private CompoundTag getRenderInfoTag() {
		return NBTHelper.getCompound(backpack, RENDER_INFO_TAG).orElse(new CompoundTag());
	}

	public Map<UpgradeRenderDataType<?>, IUpgradeRenderData> getUpgradeRenderData() {
		return upgradeData;
	}

	public void removeUpgradeRenderData(UpgradeRenderDataType<?> type) {
		upgradeData.remove(type);
		serializeUpgradeData(upgrades -> upgrades.remove(type.getName()));
		save();
	}
}
