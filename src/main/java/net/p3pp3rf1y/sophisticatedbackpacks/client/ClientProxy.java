package net.p3pp3rf1y.sophisticatedbackpacks.client;

import net.minecraftforge.eventbus.api.IEventBus;
import net.minecraftforge.fml.javafmlmod.FMLJavaModLoadingContext;
import net.p3pp3rf1y.sophisticatedbackpacks.common.CommonProxy;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;

public class ClientProxy extends CommonProxy {
	@Override
	public void registerHandlers() {
		IEventBus modBus = FMLJavaModLoadingContext.get().getModEventBus();
		ModItems.registerHandlers(modBus);
	}
}
