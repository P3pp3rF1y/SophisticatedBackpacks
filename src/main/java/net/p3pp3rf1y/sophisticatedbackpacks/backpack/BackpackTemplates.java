package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import com.google.common.collect.Sets;
import net.minecraft.ChatFormatting;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.data.CachedOutput;
import net.minecraft.data.structures.NbtToSnbt;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.SnbtPrinterTagVisitor;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.MutableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.item.Item;
import net.minecraft.world.level.storage.LevelResource;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.settings.DatapackSettingsTemplateManager;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class BackpackTemplates {
	public static final Pattern EXPORT_TEMPLATE_NAMESPACE_PATTERN = Pattern.compile("[a-z0-9_\\-\\s]+");
	public static final Pattern EXPORT_TEMPLATE_PATH_PATTERN = Pattern.compile("[a-z0-9/_\\-\\s]+");
	public static final Function<Object, MutableComponent> INVALID_CHARACTER =
			(invalid_character) ->
					Component.translatable(
							"commands.sophisticatedbackpacks.template.export.failure.invalid_characters",
							invalid_character
					).withStyle(ChatFormatting.RED);

	private BackpackTemplates() {
	}

	public static void setBackpackTemplate(ResourceLocation templateName, IBackpackWrapper wrapper) {
		Item backpackItem = wrapper.getBackpack().getItem();
		Optional<UUID> backpackUuid = wrapper.getContentsUuid();
		backpackUuid.ifPresent(uuid -> setBackpackTemplate(templateName, BuiltInRegistries.ITEM.getKey(backpackItem), BackpackStorage.get().getOrCreateBackpackContents(uuid).copy()));
	}

	public static void setBackpackTemplate(ResourceLocation templateName, ResourceLocation backpackItemRegistryName, CompoundTag contents) {
		CompoundTag data = new CompoundTag();
		data.putString("backpackItemRegistryName", backpackItemRegistryName.toString());
		data.put("backpackContents", contents);
		BackpackTemplateStorage.get().setBackpackTemplate(templateName, data);
	}

	public static Optional<CompoundTag> getBackpackTemplateNoDatapack(ResourceLocation templateName) {
		return BackpackTemplateStorage.get().getBackpackTemplate(templateName);
	}

	public static Optional<CompoundTag> getBackpackTemplate(ResourceLocation templateName) {
		Optional<CompoundTag> template = getBackpackTemplateNoDatapack(templateName);
		return template.or(() -> DatapackBackpackTemplateManager.getBackpackTemplate(templateName));
	}

	public static void removeBackpackTemplate(ResourceLocation templateName) {
		BackpackTemplateStorage.get().removeBackpackTemplate(templateName);
	}

	public static Set<ResourceLocation> getTemplateNames() {
		return getTemplateNames(true);
	}

	public static Set<ResourceLocation> getTemplateNames(boolean includeDatapackTemplates) {
		Set<ResourceLocation> templateNames = Sets.newTreeSet();
		templateNames.addAll(BackpackTemplateStorage.get().getBackpackTemplates().keySet());
		if (includeDatapackTemplates) {
			templateNames.addAll(DatapackBackpackTemplateManager.getBackpackTemplates().keySet());
		}
		return templateNames;
	}

	public static void exportTemplate(ServerPlayer player, ResourceLocation templateName, CompoundTag contentNbt) {
		Matcher matcher = EXPORT_TEMPLATE_NAMESPACE_PATTERN.matcher(templateName.getNamespace());
		if (!matcher.matches()) {
			player.displayClientMessage(INVALID_CHARACTER.apply(findNonMatchingCharacters(matcher, templateName.getNamespace())), false);
			return;
		}

		matcher = EXPORT_TEMPLATE_PATH_PATTERN.matcher(templateName.getPath());
		if (!matcher.matches()) {
			player.displayClientMessage(INVALID_CHARACTER.apply(findNonMatchingCharacters(matcher, templateName.getPath())), false);
			return;
		}

		ServerLevel serverLevel = player.serverLevel();
		Path datapacksDir = serverLevel.getServer().getWorldPath(LevelResource.DATAPACK_DIR);

		Path datapackRoot = datapacksDir.resolve(templateName.getNamespace() + "_backpack_templates");
		Path templatesDir = datapackRoot.resolve("data/" + templateName.getNamespace() + "/sophisticatedbackpacks_templates");

		if (!initDatapackStructure(datapackRoot, templatesDir)) {
			return;
		}

		String fileName = templateName.getPath();
		Path exportPath = templatesDir.resolve(fileName + ".snbt");
		try {
			NbtToSnbt.writeSnbt(CachedOutput.NO_CACHE, exportPath, (new SnbtPrinterTagVisitor()).visit(contentNbt));
		} catch (IOException e) {
			SophisticatedCore.LOGGER.error("Error writing template export", e);
			return;
		}

		DatapackSettingsTemplateManager.putTemplate(templateName.getNamespace(), fileName, contentNbt);

		player.displayClientMessage(
				Component.translatable("commands.sophisticatedbackpacks.template.export.success",
						serverLevel.getServer().getWorldPath(LevelResource.ROOT).relativize(exportPath).toString()), false
		);
	}

	public static String findNonMatchingCharacters(Matcher matcher, String input) {
		StringBuilder nonMatchingCharacters = new StringBuilder();

		for (int i = 0; i < input.length(); i++) {
			char c = input.charAt(i);
			if (!matcher.reset(String.valueOf(c)).matches()) {
				nonMatchingCharacters.append(c);
			}
		}

		return nonMatchingCharacters.toString();
	}

	private static boolean initDatapackStructure(Path datapackRoot, Path templatesDir) {
		try {
			Files.createDirectories(templatesDir);
		} catch (IOException e) {
			SophisticatedCore.LOGGER.error("Error creating directory for template export", e);
			return false;
		}
		Path packMcmetaFile = datapackRoot.resolve("pack.mcmeta");
		if (!Files.exists(packMcmetaFile)) {
			try {
				Files.writeString(packMcmetaFile, """
						{
						    "pack": {
						        "pack_format": 15,
						        "description": "Sophisticated Backpacks Templates data pack"
						    }
						}
						""");
			} catch (IOException e) {
				SophisticatedCore.LOGGER.error("Error creating pack.mcmeta for template export", e);
				return false;
			}
		}
		return true;
	}
}
