package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import net.minecraft.command.CommandSource;
import net.minecraft.command.ISuggestionProvider;
import net.minecraft.command.arguments.UUIDArgument;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

public class BackpackUUIDArgumentType extends UUIDArgument {
	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSource) {
			return ISuggestionProvider.suggest(BackpackAccessLogger.getBackpackUuids().stream().map(UUID::toString).collect(Collectors.toList()), builder);
		} else if (context.getSource() instanceof ISuggestionProvider) {
			ISuggestionProvider isuggestionprovider = (ISuggestionProvider) context.getSource();
			return isuggestionprovider.customSuggestion((CommandContext<ISuggestionProvider>) context, builder);
		}
		return Suggestions.empty();
	}

	public static BackpackUUIDArgumentType backpackUuid() {
		return new BackpackUUIDArgumentType();
	}
}
