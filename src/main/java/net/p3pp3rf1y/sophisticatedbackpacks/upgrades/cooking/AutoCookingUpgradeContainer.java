package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.cooking;

import net.minecraft.entity.player.PlayerEntity;
import net.minecraft.item.crafting.AbstractCookingRecipe;
import net.minecraft.nbt.CompoundNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.FilterLogicContainer;

public class AutoCookingUpgradeContainer<R extends AbstractCookingRecipe, W extends AutoCookingUpgradeWrapper<W, ?, R>>
		extends UpgradeContainerBase<W, AutoCookingUpgradeContainer<R, W>> {
	private final FilterLogicContainer<FilterLogic> inputFilterLogicContainer;

	private final FilterLogicContainer<FilterLogic> fuelFilterLogicContainer;
	private final CookingLogicContainer<R> cookingLogicContainer;

	public AutoCookingUpgradeContainer(PlayerEntity player, int containerId, W wrapper, UpgradeContainerType<W, AutoCookingUpgradeContainer<R, W>> type) {
		super(player, containerId, wrapper, type);
		inputFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getInputFilterLogic(), this, slots::add);
		fuelFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFuelFilterLogic(), this, slots::add);
		cookingLogicContainer = new CookingLogicContainer<>(() -> upgradeWrapper.getCookingLogic(), slots::add);
	}

	@Override
	public void handleMessage(CompoundNBT data) {
		inputFilterLogicContainer.handleMessage(data);
	}

	public CookingLogicContainer<R> getCookingLogicContainer() {
		return cookingLogicContainer;
	}

	public FilterLogicContainer<FilterLogic> getInputFilterLogicContainer() {
		return inputFilterLogicContainer;
	}

	public FilterLogicContainer<FilterLogic> getFuelFilterLogicContainer() {
		return fuelFilterLogicContainer;
	}
}
