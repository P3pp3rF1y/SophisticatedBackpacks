package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;

public class QuarkCompat implements ICompat {
	@Override
	public void setup() {
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> BackpackScreen.setScreenFactory(QuarkReadyBackpackScreen::new));
	}
}
