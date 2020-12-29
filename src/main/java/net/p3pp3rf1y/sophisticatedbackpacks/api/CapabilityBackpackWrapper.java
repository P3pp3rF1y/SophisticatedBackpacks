package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.nbt.INBT;
import net.minecraft.util.Direction;
import net.minecraftforge.common.capabilities.Capability;
import net.minecraftforge.common.capabilities.CapabilityInject;
import net.minecraftforge.common.capabilities.CapabilityManager;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.NoopBackpackWrapper;
import org.apache.commons.lang3.Validate;

import javax.annotation.Nullable;

public class CapabilityBackpackWrapper {
	private CapabilityBackpackWrapper() {}

	@CapabilityInject(IBackpackWrapper.class)
	public static final Capability<IBackpackWrapper> BACKPACK_WRAPPER_CAPABILITY = null;

	public static Capability<IBackpackWrapper> getCapabilityInstance() {
		//noinspection ConstantConditions - by this point the capability should be initialized so should only ever throw exception if something tries to use this before injection
		Validate.notNull(BACKPACK_WRAPPER_CAPABILITY, "BACKPACK_WRAPPER_CAPABILITY");

		return BACKPACK_WRAPPER_CAPABILITY;
	}

	public static void register() {
		CapabilityManager.INSTANCE.register(IBackpackWrapper.class, NoopStorage.INSTANCE, () -> NoopBackpackWrapper.INSTANCE);
	}

	public static class NoopStorage implements Capability.IStorage<IBackpackWrapper> {
		public static final NoopStorage INSTANCE = new NoopStorage();

		@Nullable
		@Override
		public INBT writeNBT(Capability<IBackpackWrapper> capability, IBackpackWrapper instance, Direction side) {
			return null;
		}

		@Override
		public void readNBT(Capability<IBackpackWrapper> capability, IBackpackWrapper instance, Direction side, INBT nbt) {
			//noop
		}
	}
}
