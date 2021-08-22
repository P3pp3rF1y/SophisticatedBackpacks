package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.nbt.CompoundNBT;

import java.util.function.Consumer;

public interface IRenderedBatteryUpgrade {
	void setBatteryRenderInfoUpdateCallback(Consumer<BatteryRenderInfo> updateTankRenderInfoCallback);

	void forceUpdateBatteryRenderInfo();

	class BatteryRenderInfo {
		private static final String CHARGE_RATIO_TAG = "chargeRatio";
		private float chargeRatio;

		public BatteryRenderInfo() {
			this(0);
		}

		public BatteryRenderInfo(float chargeRatio) {
			this.chargeRatio = chargeRatio;
		}

		public CompoundNBT serialize() {
			CompoundNBT ret = new CompoundNBT();
			ret.putFloat(CHARGE_RATIO_TAG, chargeRatio);
			return ret;
		}

		public static BatteryRenderInfo deserialize(CompoundNBT tag) {
			return new BatteryRenderInfo(tag.getFloat(CHARGE_RATIO_TAG));
		}

		public void setChargeRatio(float chargeRatio) {
			this.chargeRatio = chargeRatio;
		}

		public float getChargeRatio() {
			return chargeRatio;
		}
	}
}
