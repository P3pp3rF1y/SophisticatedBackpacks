package net.p3pp3rf1y.sophisticatedbackpacks.compat.botania;

import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.magnet.MagnetUpgradeWrapper;
import vazkii.botania.api.BotaniaAPI;

public class BotaniaCompat implements ICompat {
	@Override
	public void setup() {
		MagnetUpgradeWrapper.addMagnetPreventionChecker(itemEntity -> BotaniaAPI.instance().hasSolegnoliaAround(itemEntity));
	}
}
