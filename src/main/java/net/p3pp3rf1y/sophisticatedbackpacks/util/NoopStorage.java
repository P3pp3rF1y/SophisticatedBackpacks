package net.p3pp3rf1y.sophisticatedbackpacks.util;

import net.minecraft.nbt.INBT;
import net.minecraft.util.Direction;
import net.minecraftforge.common.capabilities.Capability;

import javax.annotation.Nullable;

public class NoopStorage implements Capability.IStorage<IBackpackWrapper> {
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
