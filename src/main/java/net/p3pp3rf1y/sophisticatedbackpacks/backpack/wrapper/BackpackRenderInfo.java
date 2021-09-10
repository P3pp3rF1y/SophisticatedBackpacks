package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedBatteryUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;
import java.util.function.Supplier;

public class BackpackRenderInfo {
	private static final String RENDER_INFO_TAG = "renderInfo";
	private static final String TANKS_TAG = "tanks";
	private static final String BATTERY_TAG = "battery";
	private static final String TANK_POSITION_TAG = "position";
	private static final String TANK_INFO_TAG = "info";

	private final ItemStack backpack;
	private final Supplier<Runnable> getBackpackSaveHandler;
	private final Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = new LinkedHashMap<>();
	@Nullable
	private IRenderedBatteryUpgrade.BatteryRenderInfo batteryRenderInfo = null;

	public BackpackRenderInfo(ItemStack backpack, Supplier<Runnable> getBackpackSaveHandler) {
		this.backpack = backpack;
		this.getBackpackSaveHandler = getBackpackSaveHandler;
		deserialize();
	}

	private void deserialize() {
		deserializeTanks();
		deserializeBattery();
	}

	private void save() {
		getBackpackSaveHandler.get().run();
	}

	public void deserializeFrom(CompoundNBT renderInfoNbt) {
		reset();
		backpack.addTagElement(RENDER_INFO_TAG, renderInfoNbt);
		deserialize();
	}

	public CompoundNBT getNbt() {
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
}
