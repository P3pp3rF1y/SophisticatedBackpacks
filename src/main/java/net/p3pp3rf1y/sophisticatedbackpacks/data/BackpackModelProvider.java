package net.p3pp3rf1y.sophisticatedbackpacks.data;

import net.minecraft.client.data.models.BlockModelGenerators;
import net.minecraft.client.data.models.ItemModelGenerators;
import net.minecraft.client.data.models.MultiVariant;
import net.minecraft.client.data.models.blockstates.MultiVariantGenerator;
import net.minecraft.client.data.models.model.ModelTemplates;
import net.minecraft.client.data.models.model.TextureMapping;
import net.minecraft.client.data.models.model.TextureSlot;
import net.minecraft.client.data.models.model.TexturedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemDisplayContext;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.client.model.generators.template.CustomLoaderBuilder;
import net.neoforged.neoforge.client.model.generators.template.ExtendedModelTemplateBuilder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.BackpackTintSources;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackBlockModel;
import net.p3pp3rf1y.sophisticatedbackpacks.client.render.BackpackItemModel;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.data.SophisticatedModelProvider;
import net.p3pp3rf1y.sophisticatedcore.upgrades.UpgradeItemBase;

import java.util.ArrayList;
import java.util.List;

public class BackpackModelProvider extends SophisticatedModelProvider {
	public BackpackModelProvider(PackOutput output) {
		super(output, SophisticatedBackpacks.MOD_ID);
	}

	@Override
	protected void registerModels(BlockModelGenerators blockModels, ItemModelGenerators itemModels) {
		List<Item> flatItems = new ArrayList<>();

		addItemClasses(flatItems, List.of(UpgradeItemBase.class));
		flatItems.add(ModItems.UPGRADE_BASE.get());
		flatItems.forEach(item -> itemModels.generateFlatItem(item, ModelTemplates.FLAT_ITEM));

		generateBackpackBlockModels(blockModels, itemModels);
	}

	protected void generateBackpackBlockModels(BlockModelGenerators blockModels, ItemModelGenerators itemModels) {
		ModItems.ITEMS.getEntries()
				.stream()
				.filter(item -> item.get() instanceof BackpackItem)
				.forEach(item -> {
					ResourceKey<Item> key = BuiltInRegistries.ITEM.getResourceKey(item.get()).orElseThrow();

					generateBackpackBlockAndItemModel(blockModels, itemModels, key, (BackpackItem) item.get());
				});
	}

	private void generateBackpackBlockAndItemModel(BlockModelGenerators blockModels, ItemModelGenerators itemModels, ResourceKey<Item> key, BackpackItem item) {
		TextureSlot clipsSlot = TextureSlot.create("clips");
		String clips;
		String backpackRegistryName = key.location().getPath();
		if (backpackRegistryName.contains("_")) {
			clips = backpackRegistryName.substring(0, backpackRegistryName.indexOf('_')) + "_clips";
		} else {
			clips = "leather_clips";
		}
		ExtendedModelTemplateBuilder modelTemplateBuilder = ExtendedModelTemplateBuilder.builder();
		if (!backpackRegistryName.contains("_")) {
			modelTemplateBuilder
					.transform(ItemDisplayContext.THIRD_PERSON_LEFT_HAND, transform -> transform.rotation(85, -90, 0).translation(0, -2, -4.5f).scale(0.75f))
					.transform(ItemDisplayContext.THIRD_PERSON_RIGHT_HAND, transform -> transform.rotation(85, -90, 0).translation(0, -2, -4.5f).scale(0.75f))
					.transform(ItemDisplayContext.FIRST_PERSON_LEFT_HAND, transform -> transform.rotation(0, 0, 0).translation(0, 0, 0).scale(0.5f))
					.transform(ItemDisplayContext.FIRST_PERSON_RIGHT_HAND, transform -> transform.rotation(0, 0, 0).translation(0, 0, 0).scale(0.5f))
					.transform(ItemDisplayContext.HEAD, transform -> transform.rotation(0, 0, 0).translation(0, 14.25f, 0).scale(1f))
					.transform(ItemDisplayContext.GUI, transform -> transform.rotation(30, 225, 0).translation(0, 1.25f, 0).scale(0.9f))
					.transform(ItemDisplayContext.GROUND, transform -> transform.rotation(0, 0, 0).translation(0, 3, 0).scale(0.5f))
					.transform(ItemDisplayContext.FIXED, transform -> transform.rotation(0, 0, 0).translation(0, 0, -2.25f).scale(0.75f))
					.transform(BackpackBlockModel.WORN, transform -> transform.rotation(0, 0, 0).translation(0, 0, 0).scale(0.99f));

		} else {
			modelTemplateBuilder.parent(SophisticatedBackpacks.getRL("backpack").withPrefix("block/"));
		}

		TexturedModel.Provider provider = TexturedModel.createDefault(b -> new TextureMapping()
				.put(clipsSlot, SophisticatedBackpacks.getRL(clips).withPrefix("block/")),
				modelTemplateBuilder.customLoader(BackpackLoaderBuilder::new, loader -> {}).requiredTextureSlot(clipsSlot).build()
		);

		Block block = item.getBackpackBlock();

		ResourceLocation blockModel = provider.create(block, blockModels.modelOutput);
		blockModels.blockStateOutput.accept(MultiVariantGenerator.dispatch(block, MultiVariant.of(new BackpackBlockStateModelBuilder(blockModel))).with(BlockModelGenerators.ROTATION_HORIZONTAL_FACING));
		blockModels.itemModelOutput.accept(item, new BackpackItemModel.Unbaked(blockModel, List.of(new BackpackTintSources.Main(BackpackWrapper.DEFAULT_MAIN_COLOR), new BackpackTintSources.Accent(BackpackWrapper.DEFAULT_ACCENT_COLOR))));
		itemModels.createFlatItemModel(item, ModelTemplates.create(key.location().toString()));
	}

	private static class BackpackLoaderBuilder extends CustomLoaderBuilder {
		protected BackpackLoaderBuilder() {
			super(ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "backpack"), false);
		}

		@Override
		protected CustomLoaderBuilder copyInternal() {
			return new BackpackLoaderBuilder();
		}
	}
}
