package net.p3pp3rf1y.sophisticatedbackpacks.registry;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import net.minecraft.client.resources.JsonReloadListener;
import net.minecraft.profiler.IProfiler;
import net.minecraft.resources.IResourceManager;
import net.minecraft.util.JSONUtils;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.fml.ModList;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.SwordRegistry;
import net.p3pp3rf1y.sophisticatedbackpacks.registry.tool.ToolRegistry;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

public class RegistryLoader extends JsonReloadListener {
	private static final Map<String, IRegistryDataLoader> loaders = new HashMap<>();
	private static final Gson GSON = new GsonBuilder().setPrettyPrinting().disableHtmlEscaping().create();

	public static void registerParser(IRegistryDataLoader parser) {
		loaders.put(parser.getName(), parser);
	}

	static {
		registerParser(new ToolRegistry.BlockToolsLoader());
		registerParser(new ToolRegistry.EntityToolsLoader());
		registerParser(new ToolRegistry.ToolTypesLoader());
		registerParser(new SwordRegistry.SwordsLoader());
	}

	private final Map<ResourceLocation, String> loadedRegistries = new HashMap<>();

	public RegistryLoader() {
		super(GSON, "registry");
	}

	private final List<DependentFile> loadLater = new ArrayList<>();

	@Override
	protected void apply(Map<ResourceLocation, JsonElement> registries, IResourceManager resourceManagerIn, IProfiler profilerIn) {
		loaders.values().forEach(IRegistryDataLoader::clear);
		registries.forEach(this::loadRegistry);
		loadDependents(registries);
	}

	private void loadDependents(Map<ResourceLocation, JsonElement> registries) {
		int lastCountLoadLater = loadLater.size();
		while (!loadLater.isEmpty()) {
			Iterator<DependentFile> iterator = loadLater.iterator();
			while (iterator.hasNext()) {
				DependentFile dependentFile = iterator.next();
				if (areDependenciesLoaded(dependentFile.getDependencies())) {
					loadRegistry(dependentFile.getName(), registries.get(dependentFile.getName()));
					iterator.remove();
				}
			}
			if (lastCountLoadLater <= loadLater.size()) {
				logIncorrectDependencies();
				break;
			}
			lastCountLoadLater = loadLater.size();
		}
	}

	private void logIncorrectDependencies() {
		for (DependentFile dependentFile : loadLater) {
			SophisticatedBackpacks.LOGGER.error("Non existent or circular load after dependencies in {} - {}", dependentFile::getName, () -> String.join(",", dependentFile.getDependencies()));
		}
	}

	private void loadRegistry(ResourceLocation name, JsonElement fullJson) {
		SophisticatedBackpacks.LOGGER.debug("Started loading registry data from {} ", name);
		String path = name.getPath();
		String shortName = path.substring(path.lastIndexOf('/') + 1);

		if (!fullJson.isJsonObject()) {
			return;
		}

		JsonObject json = fullJson.getAsJsonObject();

		Optional<IRegistryDataLoader> loader = getLoader(shortName, json);

		if (!loader.isPresent()) {
			SophisticatedBackpacks.LOGGER.error("No loader defined for {}", shortName);
			return;
		}

		if (json.has("load_after")) {
			Set<String> dependencies = JsonHelper.setFromJson(json.get("load_after"), e -> JSONUtils.convertToString(e, ""));
			if (!areDependenciesLoaded(dependencies)) {
				loadLater.add(new DependentFile(name, dependencies));
				SophisticatedBackpacks.LOGGER.debug("Registry data at {} depend on {} which are not all loaded, skipping for now.", name, dependencies);
				return;
			}
		}

		loadedRegistries.put(name, loader.get().getName());

		String modId = null;
		if (JSONUtils.isValidNode(json, "mod")) {
			modId = JSONUtils.getAsString(json, "mod");
		}

		if (isDisabled(json) || (modId != null && !ModList.get().isLoaded(modId))) {
			return;
		}

		try {
			loader.get().parse(json, modId);
			SophisticatedBackpacks.LOGGER.debug("Finished loading registry data for {}", name);
		}
		catch (Exception exception) {
			SophisticatedBackpacks.LOGGER.error("Caught exception while loading {} : {}", name, exception);
		}
	}

	private boolean areDependenciesLoaded(Set<String> dependencies) {
		for (String dependency : dependencies) {
			if (!loadedRegistries.containsValue(dependency)) {
				return false;
			}
		}
		return true;
	}

	private boolean isDisabled(JsonObject json) {
		return json.has("disabled") && JSONUtils.getAsBoolean(json, "disabled");
	}

	private Optional<IRegistryDataLoader> getLoader(String fileName, JsonObject json) {
		String parserName = fileName;
		if (json.has("type")) {
			parserName = JSONUtils.getAsString(json, "type");
		}
		return loaders.containsKey(parserName) ? Optional.of(loaders.get(parserName)) : Optional.empty();
	}

	private static class DependentFile {
		private final ResourceLocation name;
		private final Set<String> dependencies;

		private DependentFile(ResourceLocation name, Set<String> dependencies) {
			this.name = name;
			this.dependencies = dependencies;
		}

		public Set<String> getDependencies() {
			return dependencies;
		}

		public ResourceLocation getName() {
			return name;
		}
	}
}
