package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.data.DataGenerator;
import net.minecraft.data.FluidTagsProvider;
import net.minecraftforge.common.data.ExistingFileHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModFluids;

import javax.annotation.Nullable;

public class SBPFluidTagsProvider extends FluidTagsProvider {
	public SBPFluidTagsProvider(DataGenerator generatorIn,
			@Nullable ExistingFileHelper existingFileHelper) {
		super(generatorIn, SophisticatedBackpacks.MOD_ID, existingFileHelper);
	}

	@Override
	protected void addTags() {
		tag(ModFluids.EXPERIENCE_TAG).add(ModFluids.XP_STILL.get());
	}
}
