package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.client.renderer.block.model.Variant;
import net.minecraft.client.renderer.block.model.VariantMutator;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.client.model.block.CustomUnbakedBlockStateModel;
import net.neoforged.neoforge.client.model.generators.blockstate.CustomBlockStateModelBuilder;
import net.neoforged.neoforge.client.model.generators.blockstate.UnbakedMutator;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackBlockModel;

public class BackpackBlockStateModelBuilder extends CustomBlockStateModelBuilder {
	private final Variant variant;

	private BackpackBlockStateModelBuilder(Variant variant) {
		this.variant = variant;
	}

	public BackpackBlockStateModelBuilder(ResourceLocation modelLocation) {
		this(new Variant(modelLocation));
	}

	@Override
	public CustomBlockStateModelBuilder with(VariantMutator variantMutator) {
		return new BackpackBlockStateModelBuilder(variantMutator.apply(variant));
	}

	@Override
	public CustomBlockStateModelBuilder with(UnbakedMutator unbakedMutator) {
		return this;
	}

	@Override
	public CustomUnbakedBlockStateModel toUnbaked() {
		return new BackpackBlockModel.UnbakedBlockStateModel(variant);
	}
}
