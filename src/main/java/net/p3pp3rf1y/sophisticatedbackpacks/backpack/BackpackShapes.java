package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import net.minecraft.core.Direction;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.Resource;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

import javax.annotation.Nullable;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Stream;

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.*;

public class BackpackShapes {
	private BackpackShapes() {
	}

	public interface IShapeProvider {
		VoxelShape getShape(BlockState state);
	}

	private static final IShapeProvider authoritativeShapeProvider =
			new JsonModelShapeProvider(new CompositeModelJsonSource(ClasspathModelJsonSource.INSTANCE));
	private static volatile IShapeProvider defaultShapeProvider =
			authoritativeShapeProvider;
	private static volatile IShapeProvider shapeProvider = defaultShapeProvider;

	public static void setShapeProvider(IShapeProvider provider) {
		if (provider == null) {
			throw new IllegalArgumentException("Shape provider cannot be null");
		}
		shapeProvider = provider;
	}

	public static IShapeProvider getDefaultShapeProvider() {
		return defaultShapeProvider;
	}

	public static IShapeProvider getAuthoritativeShapeProvider() {
		return authoritativeShapeProvider;
	}

	public static VoxelShape getShape(BlockState state) {
		return shapeProvider.getShape(state);
	}

	public static synchronized void reloadDefaultShapeProvider(ResourceManager resourceManager) {
		applyDefaultShapeProvider(createDefaultShapeProvider(resourceManager));
	}

	static IShapeProvider createDefaultShapeProvider(ResourceManager resourceManager) {
		return new JsonModelShapeProvider(new CompositeModelJsonSource(
				new ResourceManagerModelJsonSource(resourceManager),
				ClasspathModelJsonSource.INSTANCE
		));
	}

	static synchronized void applyDefaultShapeProvider(IShapeProvider provider) {
		if (provider == null) {
			throw new IllegalArgumentException("Default shape provider cannot be null");
		}
		IShapeProvider oldDefault = defaultShapeProvider;
		defaultShapeProvider = provider;
		if (shapeProvider == oldDefault) {
			shapeProvider = provider;
		}
	}

	private static final class JsonModelShapeProvider implements IShapeProvider {
		private final ModelJsonSource modelJsonSource;
		private final Map<BackpackShapeHelper.ShapeKey, VoxelShape> shapes = new ConcurrentHashMap<>();
		private final EnumMap<BackpackShapeHelper.Part, VoxelShape> partShapes;

		private JsonModelShapeProvider(ModelJsonSource modelJsonSource) {
			this.modelJsonSource = modelJsonSource;
			this.partShapes = loadPartShapes();
		}

		private EnumMap<BackpackShapeHelper.Part, VoxelShape> loadPartShapes() {
			EnumMap<BackpackShapeHelper.Part, VoxelShape> loadedShapes = new EnumMap<>(BackpackShapeHelper.Part.class);
			Map<ResourceLocation, JsonObject> modelCache = new HashMap<>();
			for (PartModel part : PartModel.values()) {
				loadedShapes.put(part.part, loadShapeFromModel(part.modelLocation, modelCache));
			}
			return loadedShapes;
		}

		private VoxelShape loadShapeFromModel(ResourceLocation modelLocation, Map<ResourceLocation, JsonObject> modelCache) {
			JsonObject modelJson = resolveModelJson(modelLocation, modelCache);
			if (modelJson.has("elements") && modelJson.getAsJsonArray("elements").isEmpty()) {
				modelJson.remove("elements");
			}
			if (modelJson.has("elements")) {
				return parseElementsToShape(modelJson);
			}
			ResourceLocation parent = getParentLocation(modelJson, modelLocation);
			if (parent == null) {
				return Shapes.empty();
			}
			return loadShapeFromModel(parent, modelCache);
		}

		private JsonObject resolveModelJson(ResourceLocation modelLocation, Map<ResourceLocation, JsonObject> modelCache) {
			return modelCache.computeIfAbsent(modelLocation, modelJsonSource::loadModelJson);
		}

		@Nullable
		private static ResourceLocation getParentLocation(JsonObject modelJson, ResourceLocation modelLocation) {
			if (!modelJson.has("parent")) {
				return null;
			}
			String parentName = GsonHelper.getAsString(modelJson, "parent");
			if (parentName.contains(":")) {
				return ResourceLocation.parse(parentName);
			}
			return ResourceLocation.fromNamespaceAndPath(modelLocation.getNamespace(), parentName);
		}

		private static VoxelShape parseElementsToShape(JsonObject modelJson) {
			return BackpackShapeHelper.or(StreamSupportHelper.shapeStream(modelJson));
		}

		@Override
		public VoxelShape getShape(BlockState state) {
			BackpackShapeHelper.ShapeKey key = getKey(state);
			return shapes.computeIfAbsent(key, this::composeShape);
		}

		private VoxelShape composeShape(BackpackShapeHelper.ShapeKey key) {
			return BackpackShapeHelper.composeAndRotate(key, partShapes::get);
		}

		private static BackpackShapeHelper.ShapeKey getKey(BlockState state) {
			Direction dir = state.getValue(FACING);
			boolean leftTank = state.getValue(LEFT_TANK);
			boolean rightTank = state.getValue(RIGHT_TANK);
			boolean battery = state.getValue(BATTERY);
			return BackpackShapeHelper.ShapeKey.of(dir, leftTank, rightTank, battery);
		}
	}

	private interface ModelJsonSource {
		JsonObject loadModelJson(ResourceLocation modelLocation);
	}

	private static final class CompositeModelJsonSource implements ModelJsonSource {
		private final ModelJsonSource[] sources;

		private CompositeModelJsonSource(ModelJsonSource... sources) {
			this.sources = sources;
		}

		@Override
		public JsonObject loadModelJson(ResourceLocation modelLocation) {
			IllegalStateException lastError = null;
			for (ModelJsonSource source : sources) {
				try {
					JsonObject json = source.loadModelJson(modelLocation);
					if (json != null) {
						return json;
					}
				} catch (IllegalStateException e) {
					lastError = e;
				}
			}

			if (lastError != null) {
				throw lastError;
			}
			throw new IllegalStateException("Failed to load model JSON for backpack shape: " + modelLocation);
		}
	}

	private static final class ResourceManagerModelJsonSource implements ModelJsonSource {
		private final ResourceManager resourceManager;

		private ResourceManagerModelJsonSource(ResourceManager resourceManager) {
			this.resourceManager = resourceManager;
		}

		@Override
		public JsonObject loadModelJson(ResourceLocation modelLocation) {
			ResourceLocation resourceLocation = ResourceLocation.fromNamespaceAndPath(
					modelLocation.getNamespace(),
					"models/" + modelLocation.getPath() + ".json"
			);
			Optional<Resource> resource = resourceManager.getResource(resourceLocation);
			if (resource.isEmpty()) {
				throw new IllegalStateException("Missing model resource " + resourceLocation);
			}
			try (InputStream stream = resource.get().open();
				 InputStreamReader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
				return JsonParser.parseReader(reader).getAsJsonObject();
			} catch (Exception e) {
				throw new IllegalStateException("Failed to read model resource " + resourceLocation, e);
			}
		}
	}

	private static final class ClasspathModelJsonSource implements ModelJsonSource {
		private static final ClasspathModelJsonSource INSTANCE = new ClasspathModelJsonSource();

		@Override
		public JsonObject loadModelJson(ResourceLocation modelLocation) {
			String path = "assets/" + modelLocation.getNamespace() + "/models/" + modelLocation.getPath() + ".json";
			ClassLoader classLoader = BackpackShapes.class.getClassLoader();
			try (InputStream stream = classLoader.getResourceAsStream(path)) {
				if (stream == null) {
					throw new IllegalStateException("Could not find model JSON resource for shape: " + path);
				}
				try (InputStreamReader reader = new InputStreamReader(stream, StandardCharsets.UTF_8)) {
					return JsonParser.parseReader(reader).getAsJsonObject();
				}
			} catch (Exception e) {
				throw new IllegalStateException("Failed to load model JSON for backpack shape: " + modelLocation, e);
			}
		}
	}

	private enum PartModel {
		BASE(BackpackShapeHelper.Part.BASE, "block/backpack_base"),
		BATTERY(BackpackShapeHelper.Part.BATTERY, "block/backpack_battery"),
		FRONT_POUCH(BackpackShapeHelper.Part.FRONT_POUCH, "block/backpack_front_pouch"),
		LEFT_POUCH(BackpackShapeHelper.Part.LEFT_POUCH, "block/backpack_left_pouch"),
		LEFT_TANK(BackpackShapeHelper.Part.LEFT_TANK, "block/backpack_left_tank"),
		RIGHT_POUCH(BackpackShapeHelper.Part.RIGHT_POUCH, "block/backpack_right_pouch"),
		RIGHT_TANK(BackpackShapeHelper.Part.RIGHT_TANK, "block/backpack_right_tank");

		private final BackpackShapeHelper.Part part;
		private final ResourceLocation modelLocation;

		PartModel(BackpackShapeHelper.Part part, String modelPath) {
			this.part = part;
			this.modelLocation = SophisticatedBackpacks.getRL(modelPath);
		}
	}

	private static VoxelShape box(double minX, double minY, double minZ, double maxX, double maxY, double maxZ) {
		return Shapes.box(Math.min(minX, maxX), Math.min(minY, maxY), Math.min(minZ, maxZ), Math.max(minX, maxX), Math.max(minY, maxY), Math.max(minZ, maxZ));
	}

	private static final class StreamSupportHelper {
		private StreamSupportHelper() {
		}

		private static Stream<VoxelShape> shapeStream(JsonObject modelJson) {
			return modelJson.getAsJsonArray("elements").asList().stream().map(element -> {
				JsonObject elementJson = element.getAsJsonObject();
				if (elementJson.has("rotation")) {
					JsonObject rotation = elementJson.getAsJsonObject("rotation");
					if (!rotation.has("angle") || Math.abs(GsonHelper.getAsFloat(rotation, "angle")) > 0.000001f) {
						throw new IllegalStateException("Backpack shape model contains unsupported element rotation");
					}
				}
				double[] from = parseVec3(elementJson, "from");
				double[] to = parseVec3(elementJson, "to");
				return box(from[0] / 16d, from[1] / 16d, from[2] / 16d, to[0] / 16d, to[1] / 16d, to[2] / 16d);
			});
		}

		private static double[] parseVec3(JsonObject json, String key) {
			if (!json.has(key) || !json.get(key).isJsonArray() || json.getAsJsonArray(key).size() != 3) {
				throw new IllegalStateException("Backpack shape model element is missing vec3: " + key);
			}
			return new double[]{
					json.getAsJsonArray(key).get(0).getAsDouble(),
					json.getAsJsonArray(key).get(1).getAsDouble(),
					json.getAsJsonArray(key).get(2).getAsDouble()
			};
		}
	}
}
