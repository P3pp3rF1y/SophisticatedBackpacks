package net.p3pp3rf1y.sophisticatedbackpacks.command;

import com.mojang.brigadier.context.CommandContext;
import com.mojang.brigadier.suggestion.Suggestions;
import com.mojang.brigadier.suggestion.SuggestionsBuilder;
import net.minecraft.commands.CommandSourceStack;
import net.minecraft.commands.SharedSuggestionProvider;
import net.minecraft.commands.arguments.UuidArgument;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackAccessLogger;

import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public class BackpackUUIDArgumentType extends UuidArgument {
	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSourceStack) {
			return SharedSuggestionProvider.suggest(BackpackAccessLogger.getBackpackUuids().stream().map(UUID::toString).toList(), builder);
		} else if (context.getSource() instanceof SharedSuggestionProvider sharedSuggestionProvider) {
			return sharedSuggestionProvider.customSuggestion(context);
		}
		return Suggestions.empty();
	}

	public static BackpackUUIDArgumentType backpackUuid() {
		return new BackpackUUIDArgumentType();
	}
}
