package net.p3pp3rf1y.sophisticatedbackpacks.compat.trashslot;

import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.ICompat;

public class TrashSlotCompat implements ICompat {
	@Override
	public void setup() {
		net.p3pp3rf1y.sophisticatedcore.compat.trashslot.TrashSlotCompat.registerMenuType(ModItems.BACKPACK_CONTAINER_TYPE.get());
	}
}
