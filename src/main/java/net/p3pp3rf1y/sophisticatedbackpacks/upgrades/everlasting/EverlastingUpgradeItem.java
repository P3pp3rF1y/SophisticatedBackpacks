package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting;

import net.minecraft.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.UpgradeWrapperBase;

import java.util.function.Consumer;

public class EverlastingUpgradeItem extends UpgradeItemBase<EverlastingUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, EverlastingUpgradeItem> {
		public Wrapper(IBackpackWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
		}

		@Override
		public boolean hideSettingsTab() {
			return true;
		}
	}
}
