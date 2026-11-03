package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.security;

import net.minecraft.ChatFormatting;
import net.minecraft.world.entity.player.Player;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.SBPTranslationHelper;
import net.p3pp3rf1y.sophisticatedcore.api.IStorageWrapper;

import java.util.List;

public final class SecurityUpgradeHandler {
	private SecurityUpgradeHandler() {
	}

	public static boolean claimOrCheckAccess(Player player, IStorageWrapper storageWrapper) {
		List<SecurityUpgradeWrapper> securityUpgrades = storageWrapper.getUpgradeHandler().getTypeWrappers(SecurityUpgradeItem.TYPE);
		if (securityUpgrades.isEmpty()) {
			return true;
		}

		for (SecurityUpgradeWrapper securityUpgrade : securityUpgrades) {
			if (securityUpgrade.getOwnerId().isEmpty()) {
				if (!player.level().isClientSide) {
					securityUpgrade.bindTo(player);
				}
				continue;
			}
			if (!securityUpgrade.isOwnedBy(player)) {
				return false;
			}
		}
		return true;
	}

	public static boolean checkAccess(Player player, IStorageWrapper storageWrapper) {
		return storageWrapper.getUpgradeHandler().getTypeWrappers(SecurityUpgradeItem.TYPE).stream()
				.allMatch(securityUpgrade -> securityUpgrade.getOwnerId().isEmpty() || securityUpgrade.isOwnedBy(player));
	}

	public static void bindToPlayer(Player player, IStorageWrapper storageWrapper) {
		if (player.level().isClientSide) {
			return;
		}
		storageWrapper.getUpgradeHandler().getTypeWrappers(SecurityUpgradeItem.TYPE).forEach(securityUpgrade -> securityUpgrade.bindTo(player));
	}

	public static void displayAccessDenied(Player player) {
		player.displayClientMessage(SBPTranslationHelper.INSTANCE.translStatusMessage("security_upgrade_access_denied").withStyle(ChatFormatting.RED), true);
	}
}
