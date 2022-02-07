package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.nbt.CompoundTag;

import java.util.function.Consumer;

public interface IRenderedBatteryUpgrade {
	void setBatteryRenderInfoUpdateCallback(Consumer<BatteryRenderInfo> updateTankRenderInfoCallback);

	void forceUpdateBatteryRenderInfo();

	class BatteryRenderInfo {
		private static final String CHARGE_RATIO_TAG = "chargeRatio";
		private float chargeRatio;

		public BatteryRenderInfo(float chargeRatio) {
			this.chargeRatio = chargeRatio;
		}

		public CompoundTag serialize() {
			CompoundTag ret = new CompoundTag();
			ret.putFloat(CHARGE_RATIO_TAG, chargeRatio);
			return ret;
		}

		public static BatteryRenderInfo deserialize(CompoundTag tag) {
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
