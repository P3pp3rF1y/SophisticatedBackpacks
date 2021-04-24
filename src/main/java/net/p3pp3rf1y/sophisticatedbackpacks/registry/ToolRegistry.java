package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.block.AbstractRailBlock;
import net.minecraft.block.Block;
import net.minecraft.block.BlockState;
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
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
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
import java.util.function.Predicate;

public class ToolRegistry {
	private ToolRegistry() {}

	public static Map<String, ToolType> getToolTypes() {
		return TOOL_TYPES;
	}

	private static final Map<String, ToolType> TOOL_TYPES = new HashMap<>(Objects.requireNonNull(ObfuscationReflectionHelper.getPrivateValue(ToolType.class, null, "VALUES")));

	private static final Set<String> modsWithMapping = new HashSet<>();

	private static final Map<Block, Set<Item>> notBlockToolCache = new HashMap<>();

	private static final Map<Block, Set<Item>> blockTools = new HashMap<>();
	private static final Map<Block, Set<CacheableStackPredicate>> blockToolPredicates = new HashMap<>();
	private static final Map<Predicate<BlockContext>, Set<Item>> blockPredicateTools = new HashMap<>();
	private static final Map<Predicate<BlockContext>, Set<CacheableStackPredicate>> blockPredicateToolPredicates = new HashMap<>();

	private static final List<ItemMatcherFactory> itemMatcherFactories = new ArrayList<>();
	private static final List<IMatcherFactory<BlockContext>> blockMatcherFactories = new ArrayList<>();

	static {
		itemMatcherFactories.add(new ItemMatcherFactory("tag") {
			@Override
			protected Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject) {
				String tagName = JSONUtils.getString(jsonObject, "tag");
				ITag<Item> tag = TagCollectionManager.getManager().getItemTags().get(new ResourceLocation(tagName));
				return tag == null ? Optional.empty() : Optional.of(new ItemTagMatcher(tag));
			}
		});

		itemMatcherFactories.add(new ItemMatcherFactory("tool") {
			@Override
			protected Optional<CacheableStackPredicate> getPredicateFromObject(JsonObject jsonObject) {
				String toolName = JSONUtils.getString(jsonObject, "tool");
				return TOOL_TYPES.containsKey(toolName) ? Optional.of(new ToolTypeMatcher(TOOL_TYPES.get(toolName))) : Optional.empty();
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

				return Optional.of(new ModMatcher(modId));
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
	}

	public static boolean isToolForBlock(ItemStack stack, Block block, World world, BlockState state, BlockPos pos) {
		Item item = stack.getItem();
		if (blockTools.containsKey(block) && blockTools.get(block).contains(item)) {
			return true;
		}
		if (notBlockToolCache.containsKey(block) && notBlockToolCache.get(block).contains(item)) {
			return false;
		}

		AtomicBoolean shouldCache = new AtomicBoolean(true);
		if (tryToMatchAgainstBlockToolPredicates(stack, block, shouldCache)) {
			return true;
		}

		BlockContext blockContext = new BlockContext(world, state, block, pos);

		if (tryToMatchAgainstBlockPredicateTools(item, blockContext)) {
			return true;
		}

		if (tryToMatchAgainstBlockPredicateToolPredicates(stack, blockContext, shouldCache)) {
			return true;
		}

		if (tryToMatchNoMappingMod(stack, block)) {
			return true;
		}

		if (shouldCache.get()) {
			notBlockToolCache.computeIfAbsent(block, b -> new HashSet<>()).add(item);
		}

		return false;
	}

	private static boolean tryToMatchNoMappingMod(ItemStack stack, Block block) {
		if (isNoMappingModAndNonStackableItemFromSameMod(stack, block)) {
			addBlockToolMapping(block, stack.getItem());
			return true;
		}
		return false;
	}

	private static boolean isNoMappingModAndNonStackableItemFromSameMod(ItemStack stack, Block block) {
		return RegistryHelper.getRegistryName(block).map(rn ->
				!rn.getNamespace().equals("minecraft")
						&& !modsWithMapping.contains(rn.getNamespace()) && RegistryHelper.getRegistryName(stack.getItem()).map(itemRegistryName -> itemRegistryName.getNamespace().equals(rn.getNamespace())).orElse(false)
		).orElse(false) && stack.getMaxStackSize() == 1;
	}

	private static boolean tryToMatchAgainstBlockPredicateToolPredicates(ItemStack stack, BlockContext blockContext, AtomicBoolean shouldCache) {
		for (Map.Entry<Predicate<BlockContext>, Set<CacheableStackPredicate>> entry : blockPredicateToolPredicates.entrySet()) {
			if (entry.getKey().test(blockContext)) {
				Set<CacheableStackPredicate> toolPredicates = entry.getValue();
				if (tryToMatchTools(stack, blockContext.getBlock(), shouldCache, toolPredicates)) {
					return true;
				}
			}
		}
		return false;
	}

	private static boolean tryToMatchTools(ItemStack stack, Block block, AtomicBoolean shouldCache, Set<CacheableStackPredicate> toolPredicates) {
		for (CacheableStackPredicate itemPredicate : toolPredicates) {
			if (itemPredicate.preventsCaching()) {
				shouldCache.set(false);
			}
			if (itemPredicate.test(stack)) {
				if (!itemPredicate.preventsCaching()) {
					addBlockToolMapping(block, stack.getItem());
				}
				return true;
			}
		}
		return false;
	}

	private static boolean tryToMatchAgainstBlockToolPredicates(ItemStack stack, Block block, AtomicBoolean shouldCache) {
		if (blockToolPredicates.containsKey(block)) {
			Set<CacheableStackPredicate> toolPredicates = blockToolPredicates.get(block);
			return tryToMatchTools(stack, block, shouldCache, toolPredicates);
		}
		return false;
	}

	private static boolean tryToMatchAgainstBlockPredicateTools(Item item, BlockContext blockContext) {
		for (Map.Entry<Predicate<BlockContext>, Set<Item>> entry : blockPredicateTools.entrySet()) {
			if (entry.getKey().test(blockContext) && entry.getValue().contains(item)) {
				addBlockToolMapping(blockContext.getBlock(), item);
				return true;
			}
		}
		return false;
	}

	private static void addBlockToolMapping(Block block, Item item) {
		blockTools.computeIfAbsent(block, b -> new HashSet<>()).add(item);
	}

	public static class BlockToolsLoader implements IRegistryDataLoader {
		@Override
		public String getName() {
			return "block_tools";
		}

		@Override
		public void parse(JsonObject json) {
			JsonArray toolsMap = JSONUtils.getJsonArray(json, "block_tools");

			for (JsonElement jsonElement : toolsMap) {
				if (!jsonElement.isJsonObject()) {
					continue;
				}
				JsonObject entry = jsonElement.getAsJsonObject();
				parseEntry(entry);
			}
			blockTools.keySet().forEach(block -> RegistryHelper.getRegistryName(block).ifPresent(rn -> modsWithMapping.add(rn.getNamespace())));
		}

		@Override
		public void clear() {
			notBlockToolCache.clear();
			blockTools.clear();
			blockToolPredicates.clear();
			blockPredicateTools.clear();
			blockPredicateToolPredicates.clear();

			modsWithMapping.clear();
		}

		private void parseEntry(JsonObject entry) {
			if (entry.size() == 1) {
				parseFromProperty(entry);
			} else if (entry.size() == 2 && entry.has("blocks") && entry.has("tools")) {
				parseFromArrays(JSONUtils.getJsonArray(entry, "blocks"), JSONUtils.getJsonArray(entry, "tools"));
			} else {
				SophisticatedBackpacks.LOGGER.error("Invalid block tools entry - needs to have either 1 array property with mod/block name or \"blocks\" and \"tools\" array properties {}", entry);
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
					parseBlockPredicateEntry(tools, jsonElement);
				}
			}
		}

		private void parseBlockPredicateEntry(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, JsonElement jsonElement) {
			for (IMatcherFactory<BlockContext> blockMatcherFactory : blockMatcherFactories) {
				if (blockMatcherFactory.appliesTo(jsonElement)) {
					blockMatcherFactory.getPredicate(jsonElement).ifPresent(predicate -> addBlockPredicateTools(tools, predicate));
					break;
				}
			}
		}

		private void addBlockPredicateTools(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, Predicate<BlockContext> predicate) {
			tools.getA().forEach(t -> blockPredicateTools.computeIfAbsent(predicate, p -> new HashSet<>()).add(t));
			tools.getB().forEach(tp -> blockPredicateToolPredicates.computeIfAbsent(predicate, p -> new HashSet<>()).add(tp));
		}

		private void parseBlockEntry(Tuple<Set<Item>, Set<CacheableStackPredicate>> tools, String blockName) {
			ResourceLocation registryName = new ResourceLocation(blockName);
			if (ForgeRegistries.BLOCKS.containsKey(registryName)) {
				Block block = ForgeRegistries.BLOCKS.getValue(registryName);
				tools.getA().forEach(t -> blockTools.computeIfAbsent(block, b -> new HashSet<>()).add(t));
				tools.getB().forEach(tp -> blockToolPredicates.computeIfAbsent(block, b -> new HashSet<>()).add(tp));
			} else {
				SophisticatedBackpacks.LOGGER.debug("{} block doesn't exist in registry, skipping ...", blockName);
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
			ModMatcher blockPredicate = new ModMatcher(modId);
			Tuple<Set<Item>, Set<CacheableStackPredicate>> tools = getTools(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			addBlockPredicateTools(tools, blockPredicate);
		}

		private void parseBlockTools(Map.Entry<String, JsonElement> property) {
			Tuple<Set<Item>, Set<CacheableStackPredicate>> tools = getTools(property);
			if (tools.getA().isEmpty() && tools.getB().isEmpty()) {
				return;
			}
			parseBlockEntry(tools, property.getKey());
		}

		private Tuple<Set<Item>, Set<CacheableStackPredicate>> getTools(Map.Entry<String, JsonElement> property) {
			if (property.getValue().isJsonArray()) {
				JsonArray toolArray = JSONUtils.getJsonArray(property.getValue(), "");
				return getTools(toolArray);
			} else {
				SophisticatedBackpacks.LOGGER.error("Invalid tools list - needs to be an array {}", property.getValue());
				return new Tuple<>(Collections.emptySet(), Collections.emptySet());
			}
		}

		private Tuple<Set<Item>, Set<CacheableStackPredicate>> getTools(JsonArray toolArray) {
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

	private static class BlockContext {
		private final World world;

		private final BlockState state;
		private final Block block;
		private final BlockPos pos;

		public BlockContext(World world, BlockState state, Block block, BlockPos pos) {
			this.world = world;
			this.state = state;
			this.block = block;
			this.pos = pos;
		}

		public World getWorld() {
			return world;
		}

		public BlockState getState() {
			return state;
		}

		public Block getBlock() {
			return block;
		}

		public BlockPos getPos() {
			return pos;
		}
	}

	private abstract static class ItemMatcherFactory {
		private final String typeName;

		public ItemMatcherFactory(String typeName) {
			this.typeName = typeName;
		}

		public boolean appliesTo(JsonElement jsonElement) {
			return jsonElement.isJsonObject() && JSONUtils.getString(jsonElement.getAsJsonObject(), "type").equals(typeName);
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
			return jsonElement.isJsonObject() && JSONUtils.getString(jsonElement.getAsJsonObject(), "type").equals(typeName);
		}

		@Override
		public Optional<Predicate<T>> getPredicate(JsonElement jsonElement) {
			return getPredicateFromObject(jsonElement.getAsJsonObject());
		}

		protected abstract Optional<Predicate<T>> getPredicateFromObject(JsonObject jsonObject);
	}

	private interface IMatcherFactory<T> {
		boolean appliesTo(JsonElement jsonElement);

		Optional<Predicate<T>> getPredicate(JsonElement jsonElement);
	}

	private interface CacheableStackPredicate extends Predicate<ItemStack> {
		default boolean preventsCaching() {
			return false;
		}
	}

	private static class ItemTagMatcher implements CacheableStackPredicate {
		private final ITag<Item> itemTag;

		public ItemTagMatcher(ITag<Item> itemTag) {
			this.itemTag = itemTag;
		}

		@Override
		public boolean test(ItemStack stack) {
			return stack.getItem().isIn(itemTag);
		}
	}

	private static class ModMatcher implements Predicate<BlockContext> {
		private final String modId;

		public ModMatcher(String modId) {
			this.modId = modId;
			modsWithMapping.add(modId);
		}

		@Override
		public boolean test(BlockContext blockContext) {
			return RegistryHelper.getRegistryName(blockContext.getBlock()).map(rn -> rn.getNamespace().equals(modId)).orElse(false);
		}
	}

	private static class ToolTypeMatcher implements CacheableStackPredicate {
		private final ToolType toolType;

		public ToolTypeMatcher(ToolType toolType) {
			this.toolType = toolType;
		}

		@Override
		public boolean test(ItemStack stack) {
			return stack.getToolTypes().contains(toolType);
		}

		@Override
		public boolean preventsCaching() {
			return true;
		}
	}
}
