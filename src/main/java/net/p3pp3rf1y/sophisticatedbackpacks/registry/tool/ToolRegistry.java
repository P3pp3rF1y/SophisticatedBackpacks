package net.p3pp3rf1y.sophisticatedbackpacks.registry.tool;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.block.AbstractRailBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
import net.minecraft.entity.Entity;
import net.minecraft.entity.EntityType;
import net.minecraft.entity.LivingEntity;
import net.minecraft.entity.passive.AnimalEntity;
import net.minecraft.entity.passive.BeeEntity;
import net.minecraft.entity.passive.TameableEntity;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.tags.ITag;
import net.minecraft.tags.TagCollectionManager;
import net.minecraft.util.JSONUtils;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.Tuple;
import net.minecraft.util.math.BlockPos;
import net.minecraft.world.World;
import net.minecraftforge.common.ToolType;
import net.minecraftforge.fml.ModList;
import net.minecraftforge.fml.common.ObfuscationReflectionHelper;
import net.minecraftforge.items.CapabilityItemHandler;
import net.minecraftforge.registries.ForgeRegistries;
import net.minecraftforge.registries.ForgeRegistryEntry;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.IRegistryDataLoader;
import net.p3pp3rf1y.sophisticatedbackpacks.util.RegistryHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.util.WorldHelper;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class ToolRegistry {
	private ToolRegistry() {}

	public static Map<String, ToolType> getToolTypes() {
		return TOOL_TYPES;
	}

	private static final Map<String, ToolType> TOOL_TYPES = new HashMap<>(Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(ToolType.class, null, "VALUES")));

	private static final Set<String> modsWithMapping = new HashSet<>();

	private static final ToolMapping<Block, BlockContext> BLOCK_TOOL_MAPPING = new ToolMapping<>(BlockContext::getBlock);
	private static final ToolMapping<EntityType<?>, Entity> ENTITY_TOOL_MAPPING = new ToolMapping<>(Entity::getType);

	private static final List<ItemMatcherFactory> itemMatcherFactories = new ArrayList<>();
	private static final List<IMatcherFactory<BlockContext>> blockMatcherFactories = new ArrayList<>();
	private static final List<IMatcherFactory<Entity>> entityMatcherFactories = new ArrayList<>();

	static {
		itemMatcherFactories.add(new ItemMatcherFactory("tag") {
			@Override
			protected Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject) {
				String tagName = JSONUtils.getAsString(jsonObject, "tag");
				ITag<Item> tag = TagCollectionManager.getInstance().getItems().getTag(new ResourceLocation(tagName));
				return tag == null ? Optional.empty() : Optional.of(new ItemTagMatcher(tag));
			}
		});

		itemMatcherFactories.add(new ItemMatcherFactory("tool") {
			@Override
			protected Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject) {
				String toolName = JSONUtils.getAsString(jsonObject, "tool");
				return TOOL_TYPES.containsKey(toolName) ? Optional.of(new ToolTypeMatcher(TOOL_TYPES.get(toolName))) : Optional.empty();
			}
		});
		itemMatcherFactories.add(new ItemMatcherFactory("emptynbt") {
			@Override
			protected Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject) {
				ResourceLocation itemName = new ResourceLocation(JSONUtils.getAsString(jsonObject, "item"));
				if (!ForgeRegistries.ITEMS.containsKey(itemName)) {
					SophisticatedBackpacks.LOGGER.debug("{} isn't loaded in item registry, skipping ...", itemName);
				}
				Item item = ForgeRegistries.ITEMS.getValue(itemName);
				return Optional.of(st -> st.getItem() == item && (st.getTag() == null || st.getTag().isEmpty()));
			}
		});

		blockMatcherFactories.add(new IMatcherFactory<BlockContext>() {
			@Override
			public boolean appliesTo(JsonElement jsonElement) {
				return jsonElement.isJsonPrimitive();
			}

			@Override
			public Optional<Predicate<BlockContext>> getPredicate(JsonElement jsonElement) {
				String modId = jsonElement.getAsString();
				if (!ModList.get().isLoaded(modId)) {
					SophisticatedBackpacks.LOGGER.debug("{} mod isn't loaded, skipping ...", modId);
					return Optional.empty();
				}

				return Optional.of(new ModMatcher<>(modId, BlockContext::getBlock));
			}
		});
		blockMatcherFactories.add(new TypedMatcherFactory<BlockContext>("all") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(block -> true);
			}
		});
		blockMatcherFactories.add(new TypedMatcherFactory<BlockContext>("rail") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(blockContext -> blockContext.getBlock() instanceof AbstractRailBlock);
			}
		});
		blockMatcherFactories.add(new TypedMatcherFactory<BlockContext>("item_handler") {
			@Override
			protected Optional<Predicate<BlockContext>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(blockContext -> WorldHelper.getTile(blockContext.getWorld(),
						blockContext.getPos()).map(te -> te.getCapability(CapabilityItemHandler.ITEM_HANDLER_CAPABILITY).isPresent()).orElse(false));
			}
		});
		entityMatcherFactories.add(new TypedMatcherFactory<Entity>("animal") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(entity -> entity instanceof AnimalEntity);
			}
		});
		entityMatcherFactories.add(new TypedMatcherFactory<Entity>("living") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(entity -> entity instanceof LivingEntity);
			}
		});
		entityMatcherFactories.add(new TypedMatcherFactory<Entity>("bee") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(entity -> entity instanceof BeeEntity);
			}
		});
		entityMatcherFactories.add(new TypedMatcherFactory<Entity>("tameable") {
			@Override
			protected Optional<Predicate<Entity>> getPredicateFromObject(JsonObject jsonObject) {
				return Optional.of(entity -> entity instanceof TameableEntity);
			}
		});
	}

	public static boolean isToolForBlock(ItemStack stack, Block block, World world, BlockState blockState, BlockPos pos) {
		return BLOCK_TOOL_MAPPING.isToolFor(stack, block, () -> new BlockContext(world, blockState, block, pos));
	}

	public static boolean isToolForEntity(ItemStack stack, Entity entity) {
		return ENTITY_TOOL_MAPPING.isToolFor(stack, entity.getType(), () -> entity);
	}

	private abstract static class ToolsLoaderBase<T extends ForgeRegistryEntry<?>, C> implements IRegistryDataLoader {
		private final List<IMatcherFactory<C>> objectMatcherFactories;
		private final ToolMapping<T, C> toolMapping;
		private final Function<ResourceLocation, Optional<T>> getObjectFromRegistry;
		private final String name;
		private final String objectJsonArrayName;

		public ToolsLoaderBase(List<IMatcherFactory<C>> objectMatcherFactories, ToolMapping<T, C> toolMapping, Function<ResourceLocation, Optional<T>> getObjectFromRegistry, String name, String objectJsonArrayName) {
			this.objectMatcherFactories = objectMatcherFactories;
			this.toolMapping = toolMapping;
			this.getObjectFromRegistry = getObjectFromRegistry;
			this.name = name;
			this.objectJsonArrayName = objectJsonArrayName;
		}

		@Override
		public String getName() {
			return name;
		}

		@Override
		public void parse(JsonObject json) {
			JsonArray toolsMap = JSONUtils.getAsJsonArray(json, name);

			for (JsonElement jsonElement : toolsMap) {
				if (!jsonElement.isJsonObject()) {
					continue;
				}
				JsonObject entry = jsonElement.getAsJsonObject();
				parseEntry(entry);
			}
			toolMapping.getObjectTools().keySet().forEach(object -> RegistryHelper.getRegistryName(object).ifPresent(rn -> modsWithMapping.add(rn.getNamespace())));
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
				if (entry.size() == 2 && entry.has(objectJsonArrayName) && entry.has("tools")) {
					parseFromArrays(JSONUtils.getAsJsonArray(entry, objectJsonArrayName), JSONUtils.getAsJsonArray(entry, "tools"));
				} else {
					SophisticatedBackpacks.LOGGER.error("Invalid block tools entry - needs to have either 1 array property with mod/block name or \"blocks\" and \"tools\" array properties {}", entry);
				}
			}
		}

		private void parseFromArrays(JsonArray blocksArray, JsonArray toolsArray) {
			Tuple<Set<Item>, Set<CacheableStackPredicate>> tools = getTools(toolsArray);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			for (JsonElement jsonElement : blocksArray) {
				if (jsonElement.isJsonPrimitive() && jsonElement.getAsString().contains(":")) {
					parseBlockEntry(tools, jsonElement.getAsString());
				} else {
					parseObjectPredicateEntry(tools, jsonElement);
				}
			}
		}

		private void parseObjectPredicateEntry(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, JsonElement jsonElement) {
			for (IMatcherFactory<C> blockMatcherFactory : objectMatcherFactories) {
				if (blockMatcherFactory.appliesTo(jsonElement)) {
					blockMatcherFactory.getPredicate(jsonElement).ifPresent(predicate -> toolMapping.addObjectPredicateTools(tools, predicate));
					break;
				}
			}
		}

		private void parseBlockEntry(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, String objectName) {
			ResourceLocation registryName = new ResourceLocation(objectName);
			Optional<T> objectOptional = getObjectFromRegistry.apply(registryName);
			if (objectOptional.isPresent()) {
				toolMapping.addObjectTools(tools, objectOptional.get());
			} else {
				SophisticatedBackpacks.LOGGER.debug("{} doesn't exist in registry, skipping ...", objectName);
			}
		}

		private void parseFromProperty(JsonObject entry) {
			for (Map.Entry<String, JsonElement> property : entry.entrySet()) {
				if (property.getKey().contains(":")) {
					parseBlockTools(property);
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
			Tuple<Set<Item>, Set<CacheableStackPredicate>> tools = getTools(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			toolMapping.addModPredicateTools(modId, tools);
		}

		private void parseBlockTools(Map.Entry<String, JsonElement> property) {
			Tuple<Set<Item>, Set<CacheableStackPredicate>> tools = getTools(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			parseBlockEntry(tools, property.getKey());
		}

		protected Tuple<Set<Item>, Set<CacheableStackPredicate>> getTools(Map.Entry<String, JsonElement> property) {
			if (property.getValue().isJsonArray()) {
				JsonArray toolArray = JSONUtils.convertToJsonArray(property.getValue(), "");
				return getTools(toolArray);
			} else {
				SophisticatedBackpacks.LOGGER.error("Invalid tools list - needs to be an array {}", property.getValue());
				return new Tuple<>(Collections.emptySet(), Collections.emptySet());
			}
		}

		protected Tuple<Set<Item>, Set<CacheableStackPredicate>> getTools(JsonArray toolArray) {
			Set<Item> tools = new HashSet<>();
			Set<CacheableStackPredicate> toolPredicates = new HashSet<>();
			for (JsonElement jsonElement : toolArray) {
				if (jsonElement.isJsonPrimitive()) {
					ResourceLocation itemName = new ResourceLocation(jsonElement.getAsString());
					if (!ForgeRegistries.ITEMS.containsKey(itemName)) {
						SophisticatedBackpacks.LOGGER.debug("{} isn't loaded in item registry, skipping ...", itemName);
					}
					Item item = ForgeRegistries.ITEMS.getValue(itemName);
					tools.add(item);
				} else if (jsonElement.isJsonObject()) {
					for (ItemMatcherFactory itemMatcherFactory : itemMatcherFactories) {
						if (itemMatcherFactory.appliesTo(jsonElement)) {
							itemMatcherFactory.getPredicate(jsonElement).ifPresent(toolPredicates::add);
							break;
						}
					}
				}
			}
			return new Tuple<>(tools, toolPredicates);
		}
	}

	public static class BlockToolsLoader extends ToolsLoaderBase<Block, BlockContext> {
		public BlockToolsLoader() {
			super(blockMatcherFactories, BLOCK_TOOL_MAPPING, rn -> Optional.ofNullable(ForgeRegistries.BLOCKS.getValue(rn)), "block_tools", "blocks");
		}
	}

	public static class EntityToolsLoader extends ToolsLoaderBase<EntityType<?>, Entity> {
		public EntityToolsLoader() {
			super(entityMatcherFactories, ENTITY_TOOL_MAPPING, rn -> Optional.ofNullable(ForgeRegistries.ENTITIES.getValue(rn)), "entity_tools", "entities");
		}
	}

	private abstract static class ItemMatcherFactory {
		private final String typeName;

		public ItemMatcherFactory(String typeName) {
			this.typeName = typeName;
		}

		public boolean appliesTo(JsonElement jsonElement) {
			return jsonElement.isJsonObject() && JSONUtils.getAsString(jsonElement.getAsJsonObject(), "type").equals(typeName);
		}

		public Optional<CacheableStackPredicate> getPredicate(JsonElement jsonElement) {
			return getPredicateFromObject(jsonElement.getAsJsonObject());
		}

		protected abstract Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject);
	}

	private abstract static class TypedMatcherFactory<T> implements IMatcherFactory<T> {
		private final String typeName;

		public TypedMatcherFactory(String typeName) {
			this.typeName = typeName;
		}

		@Override
		public boolean appliesTo(JsonElement jsonElement) {
			return jsonElement.isJsonObject() && JSONUtils.getAsString(jsonElement.getAsJsonObject(), "type").equals(typeName);
		}

		@Override
		public Optional<Predicate<T>> getPredicate(JsonElement jsonElement) {
			return getPredicateFromObject(jsonElement.getAsJsonObject());
		}

		protected abstract Optional<Predicate<T>> getPredicateFromObject(JsonObject jsonObject);
	}

	public static void addModWithMapping(String modId) {
		modsWithMapping.add(modId);
	}

	private static class ToolMapping<T extends ForgeRegistryEntry<?>, C> {
		private final Function<C, T> getObjectFromContext;
		private final Map<T, Set<Item>> notToolCache = new HashMap<>();

		private final Map<T, Set<Item>> objectTools = new HashMap<>();
		private final Map<T, Set<CacheableStackPredicate>> objectToolPredicates = new HashMap<>();
		private final Map<Predicate<C>, Set<Item>> objectPredicateTools = new HashMap<>();
		private final Map<Predicate<C>, Set<CacheableStackPredicate>> objectPredicateToolPredicates = new HashMap<>();

		public ToolMapping(Function<C, T> getObjectFromContext) {
			this.getObjectFromContext = getObjectFromContext;
		}

		private void addObjectPredicateTools(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, Predicate<C> predicate) {
			tools.getA().forEach(t -> objectPredicateTools.computeIfAbsent(predicate, p -> new HashSet<>()).add(t));
			tools.getB().forEach(tp -> objectPredicateToolPredicates.computeIfAbsent(predicate, p -> new HashSet<>()).add(tp));
		}

		private void addObjectTools(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, T object) {
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

		public boolean isToolFor(ItemStack stack, T object, Supplier<C> getContext) {
			Item item = stack.getItem();
			if (objectTools.containsKey(object) && objectTools.get(object).contains(item)) {
				return true;
			}
			if (notToolCache.containsKey(object) && notToolCache.get(object).contains(item)) {
				return false;
			}

			AtomicBoolean shouldCache = new AtomicBoolean(true);
			if (tryToMatchAgainstBlockToolPredicates(stack, object, shouldCache)) {
				return true;
			}

			C context = getContext.get();
			if (tryToMatchAgainstBlockPredicateTools(item, context)) {
				return true;
			}

			if (tryToMatchAgainstBlockPredicateToolPredicates(stack, context, shouldCache)) {
				return true;
			}

			if (tryToMatchNoMappingMod(stack, object)) {
				return true;
			}

			if (shouldCache.get()) {
				notToolCache.computeIfAbsent(object, b -> new HashSet<>()).add(item);
			}

			return false;
		}

		private boolean tryToMatchNoMappingMod(ItemStack stack, T object) {
			if (isNoMappingModAndNonStackableItemFromSameMod(stack, object)) {
				addObjectToolMapping(object, stack.getItem());
				return true;
			}
			return false;
		}

		private boolean isNoMappingModAndNonStackableItemFromSameMod(ItemStack stack, T object) {
			return RegistryHelper.getRegistryName(object).map(rn ->
					!rn.getNamespace().equals("minecraft")
							&& !modsWithMapping.contains(rn.getNamespace()) && RegistryHelper.getRegistryName(stack.getItem()).map(itemRegistryName -> itemRegistryName.getNamespace().equals(rn.getNamespace())).orElse(false)
			).orElse(false) && stack.getMaxStackSize() == 1;
		}

		private boolean tryToMatchAgainstBlockPredicateToolPredicates(ItemStack stack, C context, AtomicBoolean shouldCache) {
			for (Map.Entry<Predicate<C>, Set<CacheableStackPredicate>> entry : objectPredicateToolPredicates.entrySet()) {
				if (entry.getKey().test(context)) {
					Set<CacheableStackPredicate> toolPredicates = entry.getValue();
					if (tryToMatchTools(stack, getObjectFromContext.apply(context), shouldCache, toolPredicates)) {
						return true;
					}
				}
			}
			return false;
		}

		private boolean tryToMatchAgainstBlockToolPredicates(ItemStack stack, T object, AtomicBoolean shouldCache) {
			if (objectToolPredicates.containsKey(object)) {
				Set<CacheableStackPredicate> toolPredicates = objectToolPredicates.get(object);
				return tryToMatchTools(stack, object, shouldCache, toolPredicates);
			}
			return false;
		}

		private boolean tryToMatchAgainstBlockPredicateTools(Item item, C context) {
			for (Map.Entry<Predicate<C>, Set<Item>> entry : objectPredicateTools.entrySet()) {
				if (entry.getKey().test(context) && entry.getValue().contains(item)) {
					addObjectToolMapping(getObjectFromContext.apply(context), item);
					return true;
				}
			}
			return false;
		}

		private boolean tryToMatchTools(ItemStack stack, T object, AtomicBoolean shouldCache, Set<CacheableStackPredicate> toolPredicates) {
			for (CacheableStackPredicate itemPredicate : toolPredicates) {
				if (itemPredicate.preventsCaching(stack)) {
					shouldCache.set(false);
				}
				if (itemPredicate.test(stack)) {
					if (!itemPredicate.preventsCaching(stack)) {
						objectTools.computeIfAbsent(object, b -> new HashSet<>()).add(stack.getItem());
					}
					return true;
				}
			}
			return false;
		}

		private void addObjectToolMapping(T block, Item item) {
			objectTools.computeIfAbsent(block, b -> new HashSet<>()).add(item);
		}

		public Map<T, Set<Item>> getObjectTools() {
			return objectTools;
		}

		public void addModPredicateTools(String modId, Tuple<Set<Item>, Set<CacheableStackPredicate>> tools) {
			addObjectPredicateTools(tools, new ModMatcher<>(modId, getObjectFromContext));
		}
	}
}
