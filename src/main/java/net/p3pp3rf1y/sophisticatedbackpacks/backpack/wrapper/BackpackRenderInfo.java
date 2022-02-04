package net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper;

import net.minecraft.nbt.CompoundTag;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.renderdata.RenderInfo;
import net.p3pp3rf1y.sophisticatedcore.util.NBTHelper;

import java.util.Optional;
import java.util.function.Supplier;

public class BackpackRenderInfo extends RenderInfo {
	private static final String RENDER_INFO_TAG = "renderInfo";
	private final ItemStack backpack;

	public BackpackRenderInfo(ItemStack backpack, Supplier<Runnable> getSaveHandler) {
		super(getSaveHandler);
		this.backpack = backpack;
		deserialize();
	}

	@Override
	protected void serializeRenderInfo(CompoundTag renderInfo) {
		NBTHelper.setCompoundNBT(backpack, RENDER_INFO_TAG, renderInfo);
	}

	@Override
	protected void deserialize() {
		super.deserialize();
	}

	@Override
	protected Optional<CompoundTag> getRenderInfoTag() {
		return NBTHelper.getCompound(backpack, RENDER_INFO_TAG);
	}
}
