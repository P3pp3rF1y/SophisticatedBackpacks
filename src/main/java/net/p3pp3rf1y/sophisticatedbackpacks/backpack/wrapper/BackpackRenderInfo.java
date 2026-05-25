package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

import java.util.Optional;
import java.util.function.Supplier;

public class BackpackRenderInfo extends RenderInfo {
	private final ItemStack backpack;

	public BackpackRenderInfo(ItemStack backpack, Supplier<Runnable> getSaveHandler) {
		super(getSaveHandler);
		this.backpack = backpack;
		deserialize();
	}

	@Override
	protected void serializeRenderInfo(CompoundTag renderInfo) {
		backpack.set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(renderInfo));
	}

	@Override
	protected Optional<CompoundTag> getRenderInfoTag() {
		CustomData renderInfo = backpack.get(ModCoreDataComponents.RENDER_INFO_TAG);
		if (renderInfo != null) {
			return Optional.of(renderInfo.copyTag());
		}

		return LegacyBackpackDataMigration.getRenderInfo(backpack).map(legacyRenderInfo -> {
			backpack.set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(legacyRenderInfo));
			return legacyRenderInfo;
		});
	}
}
