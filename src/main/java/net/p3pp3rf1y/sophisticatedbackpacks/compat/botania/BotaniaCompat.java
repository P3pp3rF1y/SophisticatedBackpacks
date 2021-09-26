package net.p3pp3rf1y.sophisticatedbackpacks.compat.botania;

import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;

public class BotaniaCompat implements ICompat {
	@Override
	public void setup() {
		//MagnetUpgradeWrapper.addMagnetPreventionChecker(itemEntity -> BotaniaAPI.instance().hasSolegnoliaAround(itemEntity)); TODO readd when Botania is on 1.17
	}
}
