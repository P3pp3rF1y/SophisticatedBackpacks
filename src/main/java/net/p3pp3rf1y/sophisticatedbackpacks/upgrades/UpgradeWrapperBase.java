package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;

import java.util.function.Consumer;

public class UpgradeWrapperBase<W extends IUpgradeWrapper, T extends UpgradeItemBase<W>> implements IUpgradeWrapper {
	protected final IBackpackWrapper backpackWrapper;
	protected final Consumer<ItemStack> upgradeSaveHandler;
	protected ItemStack upgrade;
	protected T upgradeItem;

	private long cooldown = 0;

	public UpgradeWrapperBase(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		this.backpackWrapper = backpackWrapper;
		this.upgrade = upgrade;
		//noinspection unchecked
		upgradeItem = (T) upgrade.getItem();
		this.upgradeSaveHandler = upgradeSaveHandler;
	}

	@Override
	public ItemStack getUpgradeStack() {
		return upgrade;
	}

	protected void save() {
		upgradeSaveHandler.accept(upgrade);
	}

	protected void setCooldown(World world, int time) {
		cooldown = world.getGameTime() + time;
	}

	public long getCooldownTime() {
		return cooldown;
	}

	public boolean isInCooldown(World world) {
		return getCooldownTime() > world.getGameTime();
	}
}
