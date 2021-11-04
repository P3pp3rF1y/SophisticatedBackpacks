package net.p3pp3rf1y.sophisticatedbackpacks.api;

import net.minecraft.nbt.CompoundNBT;

import java.util.Optional;
import java.util.function.Function;

public class UpgradeRenderDataType<T extends IUpgradeRenderData> {
	private final String name;
	private final Class<T> clazz;
	private final Function<CompoundNBT, T> deserialize;

	public UpgradeRenderDataType(String name, Class<T> clazz, Function<CompoundNBT, T> deserialize) {
		this.name = name;
		this.clazz = clazz;
		this.deserialize = deserialize;
	}

	public String getName() {
		return name;
	}

	public Optional<T> cast(IUpgradeRenderData upgradeRenderData) {
		if (clazz.isInstance(upgradeRenderData)) {
			return Optional.of(clazz.cast(upgradeRenderData));
		}
		return Optional.empty();
	}

	public T deserialize(CompoundNBT nbt) {
		return deserialize.apply(nbt);
	}
}
