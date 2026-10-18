package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.component.CustomData;
import net.p3pp3rf1y.sophisticatedcore.init.ModCoreDataComponents;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;

import java.util.Optional;
import java.util.function.Supplier;

public class BackpackRenderInfo extends RenderInfo {
	private final Supplier<ItemStack> backpack;
	private static final Runnable NOOP_SAVE_HANDLER = () -> {
	};

	public BackpackRenderInfo(ItemStack backpack, Supplier<Runnable> getSaveHandler) {
		this(() -> backpack, getSaveHandler);
	}

	public BackpackRenderInfo(Supplier<ItemStack> backpack, Supplier<Runnable> getSaveHandler) {
		super(getSaveHandler);
		this.backpack = backpack;
		deserialize();
	}

	public static BackpackRenderInfo fromPhysicalStack(ItemStack backpack) {
		return new BackpackRenderInfo(backpack, () -> NOOP_SAVE_HANDLER);
	}

	@Override
	protected void serializeRenderInfo(CompoundTag renderInfo) {
		backpack.get().set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(renderInfo));
	}

	@Override
	protected Optional<CompoundTag> getRenderInfoTag() {
		CustomData renderInfo = backpack.get().get(ModCoreDataComponents.RENDER_INFO_TAG);
		if (renderInfo != null) {
			return Optional.of(renderInfo.copyTag());
		}

		return LegacyBackpackDataMigration.getRenderInfo(backpack.get()).map(legacyRenderInfo -> {
			backpack.get().set(ModCoreDataComponents.RENDER_INFO_TAG, CustomData.of(legacyRenderInfo));
			return legacyRenderInfo;
		});
	}
}
