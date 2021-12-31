package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.crafting.AbstractCookingRecipe;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;

public class CookingUpgradeContainer<R extends AbstractCookingRecipe, W extends CookingUpgradeWrapper<W, ?, R>>
		extends UpgradeContainerBase<W, CookingUpgradeContainer<R, W>> {
	private final CookingLogicContainer<R> cookingLogicContainer;

	public CookingUpgradeContainer(PlayerEntity player, int containerId, W wrapper, UpgradeContainerType<W, CookingUpgradeContainer<R, W>> type) {
		super(player, containerId, wrapper, type);
		cookingLogicContainer = new CookingLogicContainer<>(() -> upgradeWrapper.getCookingLogic(), slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		//noop
	}

	public CookingLogicContainer<R> getSmeltingLogicContainer() {
		return cookingLogicContainer;
	}
}
