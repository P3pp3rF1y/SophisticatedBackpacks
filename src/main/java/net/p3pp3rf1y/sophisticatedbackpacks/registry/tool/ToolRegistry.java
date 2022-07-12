package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.core.BlockPos;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.GsonHelper;
import net.minecraft.util.Tuple;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.level.Level;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.IForgeRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.IRegistryDataLoader;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;

import javax.annotation.Nullable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToolRegistry {
	private ToolRegistry() {}

	private static final String TOOLS_PROPERTY = "tools";

	private static final Set<String> modsWithMapping = new HashSet<>();

	private static final ToolMapping<Block, BlockContext> BLOCK_TOOL_MAPPING = new ToolMapping<>(ForgeRegistries.BLOCKS, BlockContext::getBlock);
	private static final ToolMapping<EntityType<?>, Entity> ENTITY_TOOL_MAPPING = new ToolMapping<>(ForgeRegistries.ENTITY_TYPES, Entity::getType);

	public static boolean isToolForBlock(ItemStack stack, Block block, Level world, BlockState blockState, BlockPos pos) {
		return BLOCK_TOOL_MAPPING.isToolFor(stack, block, () -> new BlockContext(world, blockState, block, pos));
	}

	public static boolean isToolForEntity(ItemStack stack, Entity entity) {
		return ENTITY_TOOL_MAPPING.isToolFor(stack, entity.getType(), () -> entity);
	}

	private abstract static class ToolsLoaderBase<V, C> implements IRegistryDataLoader {
		private final List<IMatcherFactory<C>> objectMatcherFactories;
		private final ToolMapping<V, C> toolMapping;
		private final IForgeRegistry<V> registry;
		private final Function<ResourceLocation, Optional<V>> getObjectFromRegistry;
		private final String name;
		private final String objectJsonArrayName;

		public ToolsLoaderBase(List<IMatcherFactory<C>> objectMatcherFactories, ToolMapping<V, C> toolMapping, IForgeRegistry<V> registry, Function<ResourceLocation, Optional<V>> getObjectFromRegistry, String name, String objectJsonArrayName) {
			this.objectMatcherFactories = objectMatcherFactories;
			this.toolMapping = toolMapping;
			this.registry = registry;
			this.getObjectFromRegistry = getObjectFromRegistry;
			this.name = name;
			this.objectJsonArrayName = objectJsonArrayName;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public void parse(JsonObject json, @Nullable String modId) {
			JsonArray toolsMap = GsonHelper.getAsJsonArray(json, name);

			for (JsonElement jsonElement : toolsMap) {
				if (!jsonElement.isJsonObject()) {
					continue;
				}
				JsonObject entry = jsonElement.getAsJsonObject();
				parseEntry(entry);
			}
			toolMapping.getObjectTools().keySet().forEach(object -> RegistryHelper.getRegistryName(registry, object).ifPresent(rn -> modsWithMapping.add(rn.getNamespace())));
		}

		@Override
		public void clear() {
			toolMapping.clear();
			modsWithMapping.clear();
		}

		private void parseEntry(JsonObject entry) {
			if (entry.size() == 1) {
				parseFromProperty(entry);
			} else {
				if (entry.size() == 2 && entry.has(objectJsonArrayName) && entry.has(TOOLS_PROPERTY)) {
					parseFromArrays(GsonHelper.getAsJsonArray(entry, objectJsonArrayName), GsonHelper.getAsJsonArray(entry, TOOLS_PROPERTY));
				} else {
					SophisticatedBackpacks.LOGGER.error("Invalid block tools entry - needs to have either 1 array property with mod/entity name or \"{}\" and \"tools\" array properties {}", objectJsonArrayName, entry);
				}
			}
		}

		private void parseFromArrays(JsonArray blocksArray, JsonArray toolsArray) {
			Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools = getItemsAndItemPredicates(toolsArray);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			for (JsonElement jsonElement : blocksArray) {
				if (jsonElement.isJsonPrimitive() && jsonElement.getAsString().contains(":")) {
					parseObjectEntry(tools, jsonElement.getAsString());
				} else {
					parseObjectPredicateEntry(tools, jsonElement);
				}
			}
		}

		private void parseObjectPredicateEntry(Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools, JsonElement jsonElement) {
			for (IMatcherFactory<C> blockMatcherFactory : objectMatcherFactories) {
				if (blockMatcherFactory.appliesTo(jsonElement)) {
					blockMatcherFactory.getPredicate(jsonElement).ifPresent(predicate -> toolMapping.addObjectPredicateTools(tools, predicate));
					break;
				}
			}
		}

		private void parseObjectEntry(Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools, String objectName) {
			ResourceLocation registryName = new ResourceLocation(objectName);
			Optional<V> objectOptional = getObjectFromRegistry.apply(registryName);
			if (objectOptional.isPresent()) {
				toolMapping.addObjectTools(tools, objectOptional.get());
			} else {
				SophisticatedBackpacks.LOGGER.debug("{} doesn't exist in registry, skipping ...", objectName);
			}
		}

		private void parseFromProperty(JsonObject entry) {
			for (Map.Entry<String, JsonElement> property : entry.entrySet()) {
				if (property.getKey().contains(":")) {
					parseObjectTools(property);
				} else {
					parseModTools(property);
				}
			}
		}

		private void parseModTools(Map.Entry<String, JsonElement> property) {
			String modId = property.getKey();
			if (!ModList.get().isLoaded(modId)) {
				SophisticatedBackpacks.LOGGER.debug("{} mod isn't loaded, skipping ... {} ", modId, property);
				return;
			}
			Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools = getItemsAndItemPredicates(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			toolMapping.addModPredicateTools(modId, tools);
		}

		private void parseObjectTools(Map.Entry<String, JsonElement> property) {
			Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools = getItemsAndItemPredicates(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			parseObjectEntry(tools, property.getKey());
		}

	}

	protected static Tuple<Set<Item>, Set<Predicate<ItemStack>>> getItemsAndItemPredicates(Map.Entry<String, JsonElement> property) {
		if (property.getValue().isJsonArray()) {
			JsonArray toolArray = GsonHelper.convertToJsonArray(property.getValue(), "");
			return getItemsAndItemPredicates(toolArray);
		} else {
			SophisticatedBackpacks.LOGGER.error("Invalid tools list - needs to be an array {}", property.getValue());
			return new Tuple<>(Collections.emptySet(), Collections.emptySet());
		}
	}

	protected static Tuple<Set<Item>, Set<Predicate<ItemStack>>> getItemsAndItemPredicates(JsonArray toolArray) {
		Set<Item> items = new HashSet<>();
		Set<Predicate<ItemStack>> itemPredicates = new HashSet<>();
		for (JsonElement jsonElement : toolArray) {
			if (jsonElement.isJsonPrimitive()) {
				ResourceLocation itemName = new ResourceLocation(jsonElement.getAsString());
				if (!ForgeRegistries.ITEMS.containsKey(itemName)) {
					SophisticatedBackpacks.LOGGER.debug("{} isn't loaded in item registry, skipping ...", itemName);
				}
				Item item = ForgeRegistries.ITEMS.getValue(itemName);
				items.add(item);
			} else if (jsonElement.isJsonObject()) {
				Matchers.getItemMatcher(jsonElement).ifPresent(itemPredicates::add);
			}
		}
		return new Tuple<>(items, itemPredicates);
	}

	public static class BlockToolsLoader extends ToolsLoaderBase<Block, BlockContext> {
		public BlockToolsLoader() {
			super(Matchers.getBlockMatcherFactories(), BLOCK_TOOL_MAPPING, ForgeRegistries.BLOCKS, rn -> Optional.ofNullable(ForgeRegistries.BLOCKS.getValue(rn)), "block_tools", "blocks");
		}
	}

	public static class EntityToolsLoader extends ToolsLoaderBase<EntityType<?>, Entity> {
		public EntityToolsLoader() {
			super(Matchers.getEntityMatcherFactories(), ENTITY_TOOL_MAPPING, ForgeRegistries.ENTITY_TYPES, rn -> Optional.ofNullable(ForgeRegistries.ENTITY_TYPES.getValue(rn)), "entity_tools", "entities");
		}
	}

	public static void addModWithMapping(String modId) {
		modsWithMapping.add(modId);
	}

	private static class ToolMapping<V, C> {
		private final IForgeRegistry<V> registry;
		private final Function<C, V> getObjectFromContext;
		private final Map<V, Set<Item>> notToolCache = new HashMap<>();

		private final Map<V, Set<Item>> objectTools = new HashMap<>();
		private final Map<V, Set<Predicate<ItemStack>>> objectToolPredicates = new HashMap<>();
		private final Map<Predicate<C>, Set<Item>> objectPredicateTools = new HashMap<>();
		private final Map<Predicate<C>, Set<Predicate<ItemStack>>> objectPredicateToolPredicates = new HashMap<>();

		public ToolMapping(IForgeRegistry<V> registry, Function<C, V> getObjectFromContext) {
			this.registry = registry;
			this.getObjectFromContext = getObjectFromContext;
		}

		private void addObjectPredicateTools(Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools, Predicate<C> predicate) {
			tools.getA().forEach(t -> objectPredicateTools.computeIfAbsent(predicate, p -> new HashSet<>()).add(t));
			tools.getB().forEach(tp -> objectPredicateToolPredicates.computeIfAbsent(predicate, p -> new HashSet<>()).add(tp));
		}

		private void addObjectTools(Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools, V object) {
			tools.getA().forEach(t -> objectTools.computeIfAbsent(object, b -> new HashSet<>()).add(t));
			tools.getB().forEach(tp -> objectToolPredicates.computeIfAbsent(object, b -> new HashSet<>()).add(tp));
		}

		public void clear() {
			notToolCache.clear();
			objectTools.clear();
			objectToolPredicates.clear();
			objectPredicateTools.clear();
			objectPredicateToolPredicates.clear();
		}

		public boolean isToolFor(ItemStack stack, V object, Supplier<C> getContext) {
			Item item = stack.getItem();
			if (objectTools.containsKey(object) && objectTools.get(object).contains(item)) {
				return true;
			}
			if (notToolCache.containsKey(object) && notToolCache.get(object).contains(item)) {
				return false;
			}

			if (tryToMatchAgainstObjectToolPredicates(stack, object)) {
				return true;
			}

			C context = getContext.get();
			if (tryToMatchAgainstObjectPredicateTools(item, context)) {
				return true;
			}

			if (tryToMatchAgainstObjectPredicateToolPredicates(stack, context)) {
				return true;
			}

			if (tryToMatchNoMappingMod(stack, object)) {
				return true;
			}

			notToolCache.computeIfAbsent(object, b -> new HashSet<>()).add(item);

			return false;
		}

		private boolean tryToMatchNoMappingMod(ItemStack stack, V object) {
			if (isNoMappingModAndNonStackableItemFromSameMod(stack, object)) {
				addObjectToolMapping(object, stack.getItem());
				return true;
			}
			return false;
		}

		private boolean isNoMappingModAndNonStackableItemFromSameMod(ItemStack stack, V object) {
			return RegistryHelper.getRegistryName(registry, object).map(rn ->
					!rn.getNamespace().equals("minecraft")
							&& !modsWithMapping.contains(rn.getNamespace()) && RegistryHelper.getRegistryName(ForgeRegistries.ITEMS, stack.getItem()).map(itemRegistryName -> itemRegistryName.getNamespace().equals(rn.getNamespace())).orElse(false)
			).orElse(false) && stack.getMaxStackSize() == 1;
		}

		private boolean tryToMatchAgainstObjectPredicateToolPredicates(ItemStack stack, C context) {
			for (Map.Entry<Predicate<C>, Set<Predicate<ItemStack>>> entry : objectPredicateToolPredicates.entrySet()) {
				if (entry.getKey().test(context)) {
					Set<Predicate<ItemStack>> toolPredicates = entry.getValue();
					if (tryToMatchTools(stack, getObjectFromContext.apply(context), toolPredicates)) {
						return true;
					}
				}
			}
			return false;
		}

		private boolean tryToMatchAgainstObjectToolPredicates(ItemStack stack, V object) {
			if (objectToolPredicates.containsKey(object)) {
				Set<Predicate<ItemStack>> toolPredicates = objectToolPredicates.get(object);
				return tryToMatchTools(stack, object, toolPredicates);
			}
			return false;
		}

		private boolean tryToMatchAgainstObjectPredicateTools(Item item, C context) {
			for (Map.Entry<Predicate<C>, Set<Item>> entry : objectPredicateTools.entrySet()) {
				if (entry.getKey().test(context) && entry.getValue().contains(item)) {
					addObjectToolMapping(getObjectFromContext.apply(context), item);
					return true;
				}
			}
			return false;
		}

		private boolean tryToMatchTools(ItemStack stack, V object, Set<Predicate<ItemStack>> toolPredicates) {
			for (Predicate<ItemStack> itemPredicate : toolPredicates) {
				if (itemPredicate.test(stack)) {
					objectTools.computeIfAbsent(object, b -> new HashSet<>()).add(stack.getItem());
					return true;
				}
			}
			return false;
		}

		private void addObjectToolMapping(V block, Item item) {
			objectTools.computeIfAbsent(block, b -> new HashSet<>()).add(item);
		}

		public Map<V, Set<Item>> getObjectTools() {
			return objectTools;
		}

		public void addModPredicateTools(String modId, Tuple<Set<Item>, Set<Predicate<ItemStack>>> tools) {
			addObjectPredicateTools(tools, new ModMatcher<>(registry, modId, getObjectFromContext));
		}
	}
}
