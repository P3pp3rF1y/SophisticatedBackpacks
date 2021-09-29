package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.RegisterCapabilitiesEvent;
import org.apache.commons.lang3.Validate;

public class CapabilityBackpackWrapper {
	private CapabilityBackpackWrapper() {}

	@CapabilityInject(IBackpackWrapper.class)
	public static final Capability<IBackpackWrapper> BACKPACK_WRAPPER_CAPABILITY = null;

	public static Capability<IBackpackWrapper> getCapabilityInstance() {
		//noinspection ConstantConditions - by this point the capability should be initialized so should only ever throw exception if something tries to use this before injection
		Validate.notNull(BACKPACK_WRAPPER_CAPABILITY, "BACKPACK_WRAPPER_CAPABILITY");

		return BACKPACK_WRAPPER_CAPABILITY;
	}

	public static void onRegister(RegisterCapabilitiesEvent event) {
		event.register(IBackpackWrapper.class);
	}
}
