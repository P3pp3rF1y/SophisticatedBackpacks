package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import net.minecraft.item.ItemStack;
import net.minecraft.nbt.LongNBT;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.NBTHelper;

import java.util.function.Consumer;

public class UpgradeWrapperBase<W extends IUpgradeWrapper, T extends UpgradeItemBase<W>> implements IUpgradeWrapper {
	protected final Consumer<ItemStack> upgradeSaveHandler;
	protected ItemStack upgrade;
	protected T upgradeItem;

	public UpgradeWrapperBase(ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
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

	protected void setCooldown(long time) {
		upgrade.setTagInfo("cooldownTime", LongNBT.valueOf(time));
	}

	public long getCooldownTime() {
		return NBTHelper.getLong(upgrade, "cooldownTime").orElse(0L);
	}
}
