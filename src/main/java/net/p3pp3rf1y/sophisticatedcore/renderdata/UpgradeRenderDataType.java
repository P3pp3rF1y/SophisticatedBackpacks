package net.p3pp3rf1y.sophisticatedcore.renderdata;

import net.minecraft.nbt.CompoundTag;

import java.util.Optional;
import java.util.function.Function;

public class UpgradeRenderDataType<T extends IUpgradeRenderData> {
	private final String name;
	private final Class<T> clazz;
	private final Function<CompoundTag, T> deserialize;

	public UpgradeRenderDataType(String name, Class<T> clazz, Function<CompoundTag, T> deserialize) {
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

	public T deserialize(CompoundTag nbt) {
		return deserialize.apply(nbt);
	}
}
