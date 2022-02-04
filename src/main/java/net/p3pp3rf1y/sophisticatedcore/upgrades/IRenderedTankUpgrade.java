package net.p3pp3rf1y.sophisticatedcore.upgrades;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.level.material.Fluid;
import net.minecraftforge.registries.ForgeRegistries;

import javax.annotation.Nullable;
import java.util.Optional;
import java.util.function.Consumer;

public interface IRenderedTankUpgrade {
	void setTankRenderInfoUpdateCallback(Consumer<TankRenderInfo> updateTankRenderInfoCallback);

	void forceUpdateTankRenderInfo();

	class TankRenderInfo {
		private static final String FLUID_REGISTRY_NAME_TAG = "fluidRegistryName";
		private static final String FILL_RATIO_TAG = "fillRatio";

		public TankRenderInfo() {
			this(null, 0);
		}

		public TankRenderInfo(@Nullable ResourceLocation fluidRegistryName, float fillRatio) {
			this.fluidRegistryName = fluidRegistryName;
			this.fillRatio = fillRatio;
		}

		@Nullable
		private ResourceLocation fluidRegistryName;
		private float fillRatio;

		public CompoundTag serialize() {
			CompoundTag ret = new CompoundTag();
			if (fluidRegistryName != null) {
				ret.putString(FLUID_REGISTRY_NAME_TAG, fluidRegistryName.toString());
				ret.putFloat(FILL_RATIO_TAG, fillRatio);
			}
			return ret;
		}

		public static TankRenderInfo deserialize(CompoundTag tag) {
			if (tag.contains(FLUID_REGISTRY_NAME_TAG)) {
				return new TankRenderInfo(new ResourceLocation(tag.getString(FLUID_REGISTRY_NAME_TAG)), tag.getFloat(FILL_RATIO_TAG));
			}

			return new TankRenderInfo();
		}

		public void setFluid(Fluid fluid) {
			fluidRegistryName = fluid.getRegistryName();
		}

		public Optional<Fluid> getFluid() {
			return Optional.ofNullable(ForgeRegistries.FLUIDS.getValue(fluidRegistryName));
		}

		public void setFillRatio(float fillRatio) {
			this.fillRatio = fillRatio;
		}

		public float getFillRatio() {
			return fillRatio;
		}
	}
}
