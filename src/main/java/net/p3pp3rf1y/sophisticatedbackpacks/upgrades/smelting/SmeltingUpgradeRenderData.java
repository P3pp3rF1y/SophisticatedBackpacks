package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

public class SmeltingUpgradeRenderData implements IUpgradeRenderData {
	public static final UpgradeRenderDataType<SmeltingUpgradeRenderData> TYPE = new UpgradeRenderDataType<>("smelting", SmeltingUpgradeRenderData.class, SmeltingUpgradeRenderData::deserializeNBT);

	private final boolean burning;

	public SmeltingUpgradeRenderData(boolean burning) {
		this.burning = burning;
	}

	public boolean isBurning() {
		return burning;
	}

	@Override
	public CompoundTag serializeNBT() {
		return NBTHelper.putBoolean(new CompoundTag(), "burning", burning);
	}

	public static SmeltingUpgradeRenderData deserializeNBT(CompoundTag nbt) {
		return new SmeltingUpgradeRenderData(nbt.getBoolean("burning"));
	}
}
