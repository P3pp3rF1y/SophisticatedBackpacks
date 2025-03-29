package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.UuidArgument;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackTemplates;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public class BackpackTemplateArgumentType implements ArgumentType<String> {
	@Override
	public String parse(StringReader reader) throws CommandSyntaxException {
		return reader.readString();
	}

	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSourceStack) {
			return SharedSuggestionProvider.suggest(BackpackTemplates.getTemplateNames().stream().sorted(Comparator.naturalOrder()).toList(), builder);
		} else if (context.getSource() instanceof SharedSuggestionProvider sharedSuggestionProvider) {
			return sharedSuggestionProvider.customSuggestion(context);
		}
		return Suggestions.empty();
	}

	public static BackpackTemplateArgumentType templateName() {
		return new BackpackTemplateArgumentType();
	}

	@Override
	public Collection<String> getExamples() {
		return Collections.singleton("Template");
	}
}
