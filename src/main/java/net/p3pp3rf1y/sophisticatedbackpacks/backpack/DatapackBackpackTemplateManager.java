package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.common.collect.Maps;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimplePreparableReloadListener;
import net.minecraft.util.profiling.ProfilerFiller;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import org.apache.commons.io.IOUtils;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import java.util.Optional;

public class DatapackBackpackTemplateManager {
	private DatapackBackpackTemplateManager() {}

	private static final Map<ResourceLocation, CompoundTag> TEMPLATES = Maps.newTreeMap();

	private static void putBackpackTemplate(ResourceLocation templateName, CompoundTag tag) {
		TEMPLATES.put(templateName, tag);
	}

	public static Map<ResourceLocation, CompoundTag> getBackpackTemplates() {
		return TEMPLATES;
	}

	public static Optional<CompoundTag> getBackpackTemplate(ResourceLocation templateName) {
		return Optional.ofNullable(TEMPLATES.get(templateName));
	}

	public static class Loader extends SimplePreparableReloadListener<Map<ResourceLocation, CompoundTag>> {
		public static final Loader INSTANCE = new Loader();
		private static final String DIRECTORY = "sophisticatedbackpacks_templates";
		private static final String SUFFIX = ".snbt";
		private static final int PATH_SUFFIX_LENGTH = SUFFIX.length();

		private Loader() {}

		@Override
		protected Map<ResourceLocation, CompoundTag> prepare(ResourceManager resourceManager, ProfilerFiller profiler) {
			Map<ResourceLocation, CompoundTag> map = Maps.newHashMap();
			int i = DIRECTORY.length() + 1;

			resourceManager.listResources(DIRECTORY, fileName -> fileName.getPath().endsWith(SUFFIX)).forEach((resourcelocation, resource) -> {
				String s = resourcelocation.getPath();
				ResourceLocation resourceLocationWithoutSuffix = ResourceLocation.fromNamespaceAndPath(resourcelocation.getNamespace(), s.substring(i, s.length() - PATH_SUFFIX_LENGTH));

				try (
						InputStream inputstream = resource.open();
						Reader reader = new BufferedReader(new InputStreamReader(inputstream, StandardCharsets.UTF_8));
				) {
					String fileContents = IOUtils.toString(reader);

					CompoundTag tag = TagParser.parseTag(fileContents);
					if (map.put(resourceLocationWithoutSuffix, tag) != null) {
						throw new IllegalStateException("Duplicate data file ignored with ID " + resourceLocationWithoutSuffix);
					}
				}
				catch (IllegalArgumentException | IOException | CommandSyntaxException ex) {
					SophisticatedBackpacks.LOGGER.error("Couldn't parse data file {} from {}", resourceLocationWithoutSuffix, resourcelocation, ex);
				}
			});

			return map;
		}

		@Override
		protected void apply(Map<ResourceLocation, CompoundTag> templates, ResourceManager resourceManager, ProfilerFiller profiler) {
			templates.forEach(DatapackBackpackTemplateManager::putBackpackTemplate);
		}
	}
}
