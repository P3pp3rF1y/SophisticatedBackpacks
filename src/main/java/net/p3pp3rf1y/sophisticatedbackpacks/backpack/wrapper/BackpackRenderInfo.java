package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.CompoundNBT;
import net.minecraft.nbt.ListNBT;
import net.minecraftforge.common.util.Constants;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.LinkedHashMap;
import java.util.Map;

public class BackpackRenderInfo {
	private static final String RENDER_INFO_TAG = "renderInfo";
	private static final String TANKS_TAG = "tanks";
	private static final String TANK_POSITION_TAG = "position";
	private static final String TANK_INFO_TAG = "info";

	private final ItemStack backpack;
	private final Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> tankRenderInfos = new LinkedHashMap<>();

	public BackpackRenderInfo(ItemStack backpack) {
		this.backpack = backpack;
		deserialize();
	}

	private void deserialize() {
		deserializeTanks();
	}

	public void deserializeFrom(CompoundNBT renderInfoNbt) {
		reset();
		backpack.setTagInfo(RENDER_INFO_TAG, renderInfoNbt);
		deserialize();
	}

	public CompoundNBT getNbt() {
		return NBTHelper.getCompound(backpack, RENDER_INFO_TAG).orElse(new CompoundNBT());
	}

	public void reset() {
		tankRenderInfos.clear();
		NBTHelper.removeTag(backpack, RENDER_INFO_TAG);
	}

	public void setTankRenderInfo(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		tankRenderInfos.put(tankPosition, tankRenderInfo);
		serializeTank(tankPosition, tankRenderInfo);
	}

	private void deserializeTanks() {
		CompoundNBT renderInfo = NBTHelper.getCompound(backpack, RENDER_INFO_TAG).orElse(new CompoundNBT());
		ListNBT tanks = renderInfo.getList(TANKS_TAG, Constants.NBT.TAG_COMPOUND);
		for (int i = 0; i < tanks.size(); i++) {
			CompoundNBT tank = tanks.getCompound(i);
			tankRenderInfos.put(TankPosition.valueOf(tank.getString(TANK_POSITION_TAG).toUpperCase()), IRenderedTankUpgrade.TankRenderInfo.deserialize(tank.getCompound(TANK_INFO_TAG)));
		}
	}

	private void serializeTank(TankPosition tankPosition, IRenderedTankUpgrade.TankRenderInfo tankRenderInfo) {
		CompoundNBT tankInfo = tankRenderInfo.serialize();

		CompoundNBT renderInfo = NBTHelper.getCompound(backpack, RENDER_INFO_TAG).orElse(new CompoundNBT());
		ListNBT tanks = renderInfo.getList(TANKS_TAG, Constants.NBT.TAG_COMPOUND);

		boolean infoSet = false;
		for (int i = 0; i < tanks.size(); i++) {
			CompoundNBT tank = tanks.getCompound(i);
			if (tank.getString(TANK_POSITION_TAG).equals(tankPosition.getString())) {
				tank.put(TANK_INFO_TAG, tankInfo);
				infoSet = true;
			}
		}
		if (!infoSet) {
			CompoundNBT tankPositionInfo = new CompoundNBT();
			tankPositionInfo.putString(TANK_POSITION_TAG, tankPosition.getString());
			tankPositionInfo.put(TANK_INFO_TAG, tankInfo);
			tanks.add(tankPositionInfo);
			renderInfo.put(TANKS_TAG, tanks);
		}

		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
	}

	public Map<TankPosition, IRenderedTankUpgrade.TankRenderInfo> getTankRenderInfos() {
		return tankRenderInfos;
	}
}
