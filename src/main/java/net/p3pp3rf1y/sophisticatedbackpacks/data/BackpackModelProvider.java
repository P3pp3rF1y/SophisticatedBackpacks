package net.p3pp3rf1y.sophisticatedbackpacks.data;

import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import net.minecraft.client.data.models.BlockModelGenerators;
import net.minecraft.client.data.models.ItemModelGenerators;
import net.minecraft.client.data.models.MultiVariant;
import net.minecraft.client.data.models.blockstates.MultiVariantGenerator;
import net.minecraft.client.data.models.model.ModelTemplates;
import net.minecraft.client.data.models.model.TextureMapping;
import net.minecraft.client.data.models.model.TexturedModel;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.PackOutput;
import net.minecraft.resources.ResourceKey;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.block.Block;
import net.neoforged.neoforge.client.model.generators.template.CustomLoaderBuilder;
import net.neoforged.neoforge.client.model.generators.template.ExtendedModelTemplateBuilder;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.init.BackpackTintSources;
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
		String clips;
		String backpackRegistryName = key.location().getPath();
		if (backpackRegistryName.contains("_")) {
			clips = backpackRegistryName.substring(0, backpackRegistryName.indexOf('_')) + "_clips";
		} else {
			clips = "leather_clips";
		}

		TexturedModel.Provider provider = TexturedModel.createDefault(b -> new TextureMapping(),
				ExtendedModelTemplateBuilder.builder().customLoader(() -> new BackpackLoaderBuilder(clips), loader -> {
				}).build()
		);

		Block block = item.getBackpackBlock();

		ResourceLocation blockModel = provider.create(block, blockModels.modelOutput);
		blockModels.blockStateOutput.accept(MultiVariantGenerator.dispatch(block, MultiVariant.of(new BackpackBlockStateModelBuilder(blockModel))).with(BlockModelGenerators.ROTATION_HORIZONTAL_FACING));
		blockModels.itemModelOutput.accept(item, new BackpackItemModel.Unbaked(blockModel, List.of(new BackpackTintSources.Main(BackpackWrapper.DEFAULT_MAIN_COLOR), new BackpackTintSources.Accent(BackpackWrapper.DEFAULT_ACCENT_COLOR))));
		itemModels.createFlatItemModel(item, ModelTemplates.create(key.location().toString()));
	}

	private static class BackpackLoaderBuilder extends CustomLoaderBuilder {
		private final String clipsTexture;

		protected BackpackLoaderBuilder(String clipsTexture) {
			super(ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "backpack"), false);
			this.clipsTexture = clipsTexture;
		}

		@Override
		protected CustomLoaderBuilder copyInternal() {
			return new BackpackLoaderBuilder(clipsTexture);
		}

		@Override
		public JsonObject toJson(JsonObject json) {
			json = super.toJson(json);
			json.add("clipsTexture", new JsonPrimitive(ResourceLocation.fromNamespaceAndPath(SophisticatedBackpacks.MOD_ID, "block/" + clipsTexture).toString()));
			return json;
		}
	}
}
