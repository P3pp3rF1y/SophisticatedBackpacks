package net.p3pp3rf1y.sophisticatedbackpacks.init;

import net.p3pp3rf1y.sophisticatedbackpacks.compat.CompatModIds;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.curios.CuriosCompat;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.sawmill.SawmillCompat;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.trashslot.TrashSlotCompat;
import net.p3pp3rf1y.sophisticatedcore.compat.CompatInfo;
import net.p3pp3rf1y.sophisticatedcore.compat.CompatRegistry;

public class ModCompat {
	private ModCompat() {
	}

	public static void register() {
		CompatRegistry.registerCompat(new CompatInfo(CompatModIds.CURIOS, null), () -> modBus -> new CuriosCompat());
		// CompatRegistry.registerCompat(new CompatInfo(CompatModIds.BOTANIA, null), () -> BotaniaCompat::new);
		CompatRegistry.registerCompat(new CompatInfo(CompatModIds.SAWMILL, null), () -> modBus -> new SawmillCompat());
		CompatRegistry.registerCompat(new CompatInfo(net.p3pp3rf1y.sophisticatedcore.compat.CompatModIds.TRASH_SLOT), () -> modBus -> new TrashSlotCompat());
		// CompatRegistry.registerCompat(new CompatInfo(CompatModIds.CHIPPED, null), () -> modBus -> new ChippedCompat());
		// TODO re-enable when Accessories is updated
		// CompatRegistry.registerCompat(new CompatInfo(CompatModIds.ACCESSORIES, null), () -> modBus -> new AccessoriesCompat());
	}
}
