package net.p3pp3rf1y.sophisticatedbackpacks.compat.quark;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.fml.DistExecutor;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.compat.ICompat;
import net.p3pp3rf1y.sophisticatedbackpacks.network.PacketHandler;
import vazkii.quark.base.module.ModuleLoader;
import vazkii.quark.content.management.module.InventorySortingModule;

public class QuarkCompat implements ICompat {
	@Override
	public void setup() {
		DistExecutor.unsafeRunWhenOn(Dist.CLIENT, () -> () -> {
			if (ModuleLoader.INSTANCE.isModuleEnabled(InventorySortingModule.class)) {
				BackpackScreen.setButtonReplacer(new QuarkButtonReplacer());
			}
		});
		PacketHandler.registerMessage(TransferMessage.class, TransferMessage::encode, TransferMessage::decode, TransferMessage::onMessage);
	}

}
