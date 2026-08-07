package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.everlasting;

import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.Config;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import java.util.List;
import java.util.function.Consumer;

public class EverlastingUpgradeItem extends UpgradeItemBase<EverlastingUpgradeItem.Wrapper> {
	public static final UpgradeType<Wrapper> TYPE = new UpgradeType<>(Wrapper::new);
	public static final List<UpgradeConflictDefinition> UPGRADE_CONFLICT_DEFINITIONS = List.of(
			new UpgradeConflictDefinition(EverlastingUpgradeItem.class::isInstance, 0, SBPTranslationHelper.INSTANCE.translError("add.everlasting_exists")));

	public EverlastingUpgradeItem() {
		super(Config.SERVER.maxUpgradesPerStorage);
	}

	@Override
	public UpgradeType<Wrapper> getType() {
		return TYPE;
	}

	@Override
	public List<UpgradeConflictDefinition> getUpgradeConflicts() {
		return UPGRADE_CONFLICT_DEFINITIONS;
	}

	public static class Wrapper extends UpgradeWrapperBase<Wrapper, EverlastingUpgradeItem> {
		public Wrapper(IStorageWrapper backpackWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
			super(backpackWrapper, upgrade, upgradeSaveHandler);
		}

		@Override
		public boolean hideSettingsTab() {
			return true;
		}

		@Override
		public boolean canBeDisabled() {
			return false;
		}
	}
}
