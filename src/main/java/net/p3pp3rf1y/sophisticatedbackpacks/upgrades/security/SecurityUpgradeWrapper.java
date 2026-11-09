package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.security;

import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModDataComponents;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeWrapperBase;

import java.util.Optional;
import java.util.UUID;
import java.util.function.Consumer;

public class SecurityUpgradeWrapper extends UpgradeWrapperBase<SecurityUpgradeWrapper, SecurityUpgradeItem> {
	public SecurityUpgradeWrapper(IStorageWrapper storageWrapper, ItemStack upgrade, Consumer<ItemStack> upgradeSaveHandler) {
		super(storageWrapper, upgrade, upgradeSaveHandler);
	}

	public Optional<UUID> getOwnerId() {
		return Optional.ofNullable(upgrade.get(ModDataComponents.SECURITY_OWNER));
	}

	public void bindTo(Player player) {
		if (getOwnerId().isPresent()) {
			return;
		}
		upgrade.set(ModDataComponents.SECURITY_OWNER, player.getUUID());
		upgrade.set(ModDataComponents.SECURITY_OWNER_NAME, player.getGameProfile().getName());
		save();
	}

	public boolean isOwnedBy(Player player) {
		return getOwnerId().filter(player.getUUID()::equals).isPresent();
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
