package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.StringReader;
import com.mojang.brigadier.arguments.ArgumentType;
import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.exceptions.CommandSyntaxException;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class BackpackPlayerArgumentType implements ArgumentType<String> {
	@Override
	public String parse(StringReader reader) throws CommandSyntaxException {
		return reader.readString();
	}

	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSource) {
			return ISuggestionProvider.suggest(BackpackAccessLogger.getPlayerNames().stream().sorted(Comparator.naturalOrder()).collect(Collectors.toList()), builder);
		} else if (context.getSource() instanceof ISuggestionProvider) {
			ISuggestionProvider isuggestionprovider = (ISuggestionProvider) context.getSource();
			return isuggestionprovider.customSuggestion((CommandContext<ISuggestionProvider>) context, builder);
		}
		return Suggestions.empty();
	}

	public static BackpackPlayerArgumentType playerName() {
		return new BackpackPlayerArgumentType();
	}

	@Override
	public Collection<String> getExamples() {
		return Collections.singleton("Player");
	}
}
