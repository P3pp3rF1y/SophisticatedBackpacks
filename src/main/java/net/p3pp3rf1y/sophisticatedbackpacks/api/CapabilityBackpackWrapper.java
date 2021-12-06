package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.minecraftforge.common.capabilities.CapabilityToken;
import net.minecraftforge.common.capabilities.RegisterCapabilitiesEvent;

public class CapabilityBackpackWrapper {
	private CapabilityBackpackWrapper() {}

	public static final Capability<IBackpackWrapper> BACKPACK_WRAPPER_CAPABILITY = CapabilityManager.get(new CapabilityToken<>() {});

	public static Capability<IBackpackWrapper> getCapabilityInstance() {
		return BACKPACK_WRAPPER_CAPABILITY;
	}

	public static void onRegister(RegisterCapabilitiesEvent event) {
		event.register(IBackpackWrapper.class);
	}
}
