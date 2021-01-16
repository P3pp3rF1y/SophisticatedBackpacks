package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.world.World;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public abstract class UpgradeWrapperBase<W extends IUpgradeWrapper, T extends UpgradeItemBase<W>> implements IUpgradeWrapper {
	protected final IBackpackWrapper backpackWrapper;
	protected final Consumer<ItemStack> upgradeSaveHandler;
	protected ItemStack upgrade;
	protected T upgradeItem;

	private long cooldown = 0;

	protected UpgradeWrapperBase(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
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

	@Override
	public boolean isEnabled() {
		return NBTHelper.getBoolean(upgrade, "enabled").orElse(true);
	}

	@Override
	public void setEnabled(boolean enabled) {
		NBTHelper.setBoolean(upgrade, "enabled", enabled);
		save();
		backpackWrapper.getUpgradeHandler().refreshUpgradeWrappers();
	}
}
