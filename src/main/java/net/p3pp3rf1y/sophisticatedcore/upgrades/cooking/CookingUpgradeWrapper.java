package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraft.core.BlockPos;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.minecraft.world.item.crafting.BlastingRecipe;
import net.minecraft.world.item.crafting.RecipeType;
import net.minecraft.world.item.crafting.SmeltingRecipe;
import net.minecraft.world.item.crafting.SmokingRecipe;
import net.minecraft.world.level.Level;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.upgrades.ITickableUpgrade;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import javax.annotation.Nullable;
import java.util.function.Consumer;

public abstract class CookingUpgradeWrapper<W extends CookingUpgradeWrapper<W, U, R>, U extends UpgradeItemBase<W> & ICookingUpgradeItem, R extends AbstractCookingRecipe>
		extends UpgradeWrapperBase<W, U> implements ITickableUpgrade, ICookingUpgrade<R> {
	private static final int NOTHING_TO_DO_COOLDOWN = 10;
	protected final CookingLogic<R> cookingLogic;

	protected CookingUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler, RecipeType<R> recipeType, float burnTimeModifier) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
		cookingLogic = new CookingLogic<>(upgrade, upgradeSaveHandler, upgradeItem.getCookingUpgradeConfig(), recipeType, burnTimeModifier);
	}

	public void tick(@Nullable LivingEntity entity, Level world, BlockPos pos) {
		if (isInCooldown(world)) {
			return;
		}

		if (!cookingLogic.tick(world)) {
			setCooldown(world, NOTHING_TO_DO_COOLDOWN);
		}

		boolean isBurning = cookingLogic.isBurning(world);
		RenderInfo renderInfo = storageWrapper.getRenderInfo();
		if (renderInfo.getUpgradeRenderData(CookingUpgradeRenderData.TYPE).map(CookingUpgradeRenderData::isBurning).orElse(false) != isBurning) {
			if (isBurning) {
				renderInfo.setUpgradeRenderData(CookingUpgradeRenderData.TYPE, new CookingUpgradeRenderData(true));
			} else {
				renderInfo.removeUpgradeRenderData(CookingUpgradeRenderData.TYPE);
			}
		}
	}

	@Override
	public void setEnabled(boolean enabled) {
		if (!enabled) {
			pauseAndRemoveRenderInfo();
		}
		super.setEnabled(enabled);
	}

	@Override
	public void onBeforeRemoved() {
		pauseAndRemoveRenderInfo();
	}

	private void pauseAndRemoveRenderInfo() {
		cookingLogic.pause();
		RenderInfo renderInfo = storageWrapper.getRenderInfo();
		renderInfo.removeUpgradeRenderData(CookingUpgradeRenderData.TYPE);
	}

	public CookingLogic<R> getCookingLogic() {
		return cookingLogic;
	}

	public static class SmeltingUpgradeWrapper extends CookingUpgradeWrapper<SmeltingUpgradeWrapper, SmeltingUpgradeItem, SmeltingRecipe> {
		public SmeltingUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(storageWrapper, upgrade, upgradeSaveHandler, RecipeType.SMELTING, 1);
		}
	}

	public static class SmokingUpgradeWrapper extends CookingUpgradeWrapper<SmokingUpgradeWrapper, SmokingUpgradeItem, SmokingRecipe> {
		public SmokingUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(storageWrapper, upgrade, upgradeSaveHandler, RecipeType.SMOKING, 0.5f);
		}
	}

	public static class BlastingUpgradeWrapper extends CookingUpgradeWrapper<BlastingUpgradeWrapper, BlastingUpgradeItem, BlastingRecipe> {
		public BlastingUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(storageWrapper, upgrade, upgradeSaveHandler, RecipeType.BLASTING, 0.5f);
		}
	}
}
