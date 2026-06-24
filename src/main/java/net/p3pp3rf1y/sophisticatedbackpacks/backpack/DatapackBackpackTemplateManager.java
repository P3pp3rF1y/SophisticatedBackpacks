package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.common.collect.Maps;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import net.minecraft.nbt.NbtOps;
import net.minecraft.nbt.TagParser;
import net.minecraft.resources.Identifier;
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
	private DatapackBackpackTemplateManager() {
	}

	private static final Map<Identifier, BackpackTemplate> TEMPLATES = Maps.newTreeMap();

	private static void putBackpackTemplate(Identifier templateName, BackpackTemplate tag) {
		TEMPLATES.put(templateName, tag);
	}

	public static Map<Identifier, BackpackTemplate> getBackpackTemplates() {
		return TEMPLATES;
	}

	public static Optional<BackpackTemplate> getBackpackTemplate(Identifier templateName) {
		return Optional.ofNullable(TEMPLATES.get(templateName));
	}

	public static class Loader extends SimplePreparableReloadListener<Map<Identifier, BackpackTemplate>> {
		public static final Identifier KEY = SophisticatedBackpacks.getIdentifier("template_loader");
		public static final Loader INSTANCE = new Loader();
		private static final String DIRECTORY = "sophisticatedbackpacks_templates";
		private static final String SUFFIX = ".snbt";
		private static final int PATH_SUFFIX_LENGTH = SUFFIX.length();

		private Loader() {
		}

		@Override
		protected Map<Identifier, BackpackTemplate> prepare(ResourceManager resourceManager, ProfilerFiller profiler) {
			Map<Identifier, BackpackTemplate> map = Maps.newHashMap();
			int i = DIRECTORY.length() + 1;

			resourceManager.listResources(DIRECTORY, fileName -> fileName.getPath().endsWith(SUFFIX)).forEach((identifier, resource) -> {
				String s = identifier.getPath();
				Identifier identifierWithoutSuffix = Identifier.fromNamespaceAndPath(identifier.getNamespace(),
						s.substring(i, s.length() - PATH_SUFFIX_LENGTH));

				try (InputStream inputstream = resource.open();
						Reader reader = new BufferedReader(new InputStreamReader(inputstream, StandardCharsets.UTF_8));) {
					String fileContents = IOUtils.toString(reader);

					BackpackTemplate template = BackpackTemplate.CODEC
							.decode(getRegistryLookup().createSerializationContext(NbtOps.INSTANCE), TagParser.parseCompoundFully(fileContents)).getOrThrow()
							.getFirst();
					if (map.put(identifierWithoutSuffix, template) != null) {
						throw new IllegalStateException("Duplicate data file ignored with ID " + identifierWithoutSuffix);
					}
				} catch (IllegalArgumentException | IOException | CommandSyntaxException ex) {
					SophisticatedBackpacks.LOGGER.error("Couldn't parse data file {} from {}", identifierWithoutSuffix, identifier, ex);
				}
			});

			return map;
		}

		@Override
		protected void apply(Map<Identifier, BackpackTemplate> templates, ResourceManager resourceManager, ProfilerFiller profiler) {
			templates.forEach(DatapackBackpackTemplateManager::putBackpackTemplate);
		}
	}
}
