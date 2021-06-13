package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.tank;

import net.minecraft.fluid.Fluids;
import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IRenderedTankUpgrade;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class TankUpgradeItem extends UpgradeItemBase<TankUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, TankUpgradeItem> implements IRenderedTankUpgrade {
		private Consumer<TankRenderInfo> updateTankRenderInfoCallback;

		protected Wrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
		}

		@Override
		public void setTankRenderInfoUpdateCallback(Consumer<TankRenderInfo> updateTankRenderInfoCallback) {
			this.updateTankRenderInfoCallback = updateTankRenderInfoCallback;
		}

		@Override
		public void forceUpdateTankRenderInfo() {
			TankRenderInfo renderInfo = new TankRenderInfo();
			renderInfo.setFluid(Fluids.LAVA);
			renderInfo.setFillRatio(0.5f);
			updateTankRenderInfoCallback.accept(renderInfo);
		}
	}
}
