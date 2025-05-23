package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
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

public class BackpackTemplateArgumentType implements ArgumentType<ResourceLocation> {
	private final boolean includeDatapackTemplates;

	public BackpackTemplateArgumentType(boolean includeDatapackTemplates) {
		this.includeDatapackTemplates = includeDatapackTemplates;
	}

	@Override
	public ResourceLocation parse(StringReader reader) throws CommandSyntaxException {
		return ResourceLocation.read(reader);
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
