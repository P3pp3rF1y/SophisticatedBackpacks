package net.p3pp3rf1y.sophisticatedcore.compat.craftingtweaks;

import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;

public class CraftingTweaksCompat implements ICompat {
	@Override
	public void setup() {
		//CraftingTweaksAPI.registerCraftingGridProvider(new CraftingUpgradeTweakProvider());
		//DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CraftingUpgradeTweakUIPart::register);
	}
}
