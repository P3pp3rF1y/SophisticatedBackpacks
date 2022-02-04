package net.p3pp3rf1y.sophisticatedcore.upgrades.cooking;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.crafting.AbstractCookingRecipe;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogic;
import net.p3pp3rf1y.sophisticatedcore.upgrades.FilterLogicContainer;

public class AutoCookingUpgradeContainer<R extends AbstractCookingRecipe, W extends AutoCookingUpgradeWrapper<W, ?, R>>
		extends UpgradeContainerBase<W, AutoCookingUpgradeContainer<R, W>> {
	private final FilterLogicContainer<FilterLogic> inputFilterLogicContainer;

	private final FilterLogicContainer<FilterLogic> fuelFilterLogicContainer;
	private final CookingLogicContainer<R> cookingLogicContainer;

	public AutoCookingUpgradeContainer(Player player, int containerId, W wrapper, UpgradeContainerType<W, AutoCookingUpgradeContainer<R, W>> type) {
		super(player, containerId, wrapper, type);
		inputFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getInputFilterLogic(), this, slots::add);
		fuelFilterLogicContainer = new FilterLogicContainer<>(() -> upgradeWrapper.getFuelFilterLogic(), this, slots::add);
		cookingLogicContainer = new CookingLogicContainer<>(() -> upgradeWrapper.getCookingLogic(), slots::add);
	}

	@Override
	public void handleMessage(CompoundTag data) {
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
