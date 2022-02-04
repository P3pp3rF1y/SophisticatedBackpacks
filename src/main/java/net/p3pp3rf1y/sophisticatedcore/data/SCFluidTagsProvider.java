package net.p3pp3rf1y.sophisticatedcore.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.tags.FluidTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.init.ModFluids;

import javax.annotation.Nullable;

public class SCFluidTagsProvider extends FluidTagsProvider {
	public SCFluidTagsProvider(DataGenerator generatorIn,
			@Nullable ExistingFileHelper existingFileHelper) {
		super(generatorIn, SophisticatedCore.MOD_ID, existingFileHelper);
	}

	@Override
	protected void addTags() {
		tag(ModFluids.EXPERIENCE_TAG).add(ModFluids.XP_STILL.get());
	}
}
