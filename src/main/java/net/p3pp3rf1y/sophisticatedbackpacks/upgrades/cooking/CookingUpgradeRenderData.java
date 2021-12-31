package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

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
	public CompoundNBT serializeNBT() {
		return NBTHelper.putBoolean(new CompoundNBT(), "burning", burning);
	}

	public static CookingUpgradeRenderData deserializeNBT(CompoundNBT nbt) {
		return new CookingUpgradeRenderData(nbt.getBoolean("burning"));
	}
}
