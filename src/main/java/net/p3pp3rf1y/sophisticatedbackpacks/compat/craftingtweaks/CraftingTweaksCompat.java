package net.p3pp3rf1y.sophisticatedbackpacks.compat.craftingtweaks;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;

public class CraftingTweaksCompat implements ICompat {
	@Override
	public void setup() {
		//CraftingTweaksAPI.registerProvider(BackpackContainer.class, new CraftingUpgradeTweakProvider()); TODO readd with crafting tweaks when they are on 1.17
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CraftingUpgradeTweakUIPart::register);
	}
}
