package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraft.nbt.CompoundTag;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

public class CookingUpgradeRenderData implements IUpgradeRenderData {
	public static final UpgradeRenderDataType<CookingUpgradeRenderData> TYPE = new UpgradeRenderDataType<>("smelting", CookingUpgradeRenderData.class, CookingUpgradeRenderData::deserializeNBT);

	private final boolean burning;

	public CookingUpgradeRenderData(boolean burning) {
		this.burning = burning;
	}

	public boolean isBurning() {
		return burning;
	}

	@Override
	public CompoundTag serializeNBT() {
		return NBTHelper.putBoolean(new CompoundTag(), "burning", burning);
	}

	public static CookingUpgradeRenderData deserializeNBT(CompoundTag nbt) {
		return new CookingUpgradeRenderData(nbt.getBoolean("burning"));
	}
}
