package net.p3pp3rf1y.sophisticatedbackpacks.compat.craftingtweaks;

import net.blay09.mods.craftingtweaks.api.CraftingTweaksAPI;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;

public class CraftingTweaksCompat implements ICompat {
	@Override
	public void setup() {
		CraftingTweaksAPI.registerProvider(BackpackContainer.class, new CraftingUpgradeTweakProvider());
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> CraftingUpgradeTweakUIPart::register);
	}
}
