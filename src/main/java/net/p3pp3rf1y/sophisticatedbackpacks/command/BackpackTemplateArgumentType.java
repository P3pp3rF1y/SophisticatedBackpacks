package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.exceptions.DynamicCommandExceptionType;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.resources.ResourceLocation;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.CompletableFuture;
import java.util.regex.Matcher;

public class BackpackTemplateArgumentType implements ArgumentType<ResourceLocation> {
	private static final DynamicCommandExceptionType ERROR_INVALID =
			new DynamicCommandExceptionType(BackpackTemplates.INVALID_CHARACTER::apply);

	private final boolean includeDatapackTemplates;

	public BackpackTemplateArgumentType(boolean includeDatapackTemplates) {
		this.includeDatapackTemplates = includeDatapackTemplates;
	}

	@Override
	public ResourceLocation parse(StringReader reader) throws CommandSyntaxException {
		ResourceLocation templateName = ResourceLocation.read(reader);

		Matcher matcher = BackpackTemplates.EXPORT_TEMPLATE_NAMESPACE_PATTERN.matcher(templateName.getNamespace());
		if (!matcher.matches()) {
			throw ERROR_INVALID.createWithContext(reader, BackpackTemplates.findNonMatchingCharacters(matcher, templateName.getNamespace()));
		}

		matcher = BackpackTemplates.EXPORT_TEMPLATE_PATH_PATTERN.matcher(templateName.getPath());
		if (!matcher.matches()) {
			throw ERROR_INVALID.createWithContext(reader, BackpackTemplates.findNonMatchingCharacters(matcher, templateName.getPath()));
		}

		return templateName;
	}

	public static ResourceLocation getId(CommandContext<CommandSourceStack> context, String name) {
		return context.getArgument(name, ResourceLocation.class);
	}

	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSourceStack) {
			return SharedSuggestionProvider.suggest(
					BackpackTemplates.getTemplateNames(includeDatapackTemplates).stream()
							.map(ResourceLocation::toString)
							.sorted(Comparator.naturalOrder())
							.toList(),
					builder);
		} else if (context.getSource() instanceof SharedSuggestionProvider sharedSuggestionProvider) {
			return sharedSuggestionProvider.customSuggestion(context);
		}
		return Suggestions.empty();
	}

	public static BackpackTemplateArgumentType templateName() {
		return new BackpackTemplateArgumentType(true);
	}

	public static BackpackTemplateArgumentType templateName(boolean includeDatapackTemplates) {
		return new BackpackTemplateArgumentType(includeDatapackTemplates);
	}

	@Override
	public Collection<String> getExamples() {
		return Collections.singleton("Template");
	}
}
