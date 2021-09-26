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
import java.util.stream.Collectors;

public class BackpackUUIDArgumentType extends UuidArgument {
	@Override
	public <S> CompletableFuture<Suggestions> listSuggestions(CommandContext<S> context, SuggestionsBuilder builder) {
		if (context.getSource() instanceof CommandSourceStack) {
			return SharedSuggestionProvider.suggest(BackpackAccessLogger.getBackpackUuids().stream().map(UUID::toString).collect(Collectors.toList()), builder);
		} else if (context.getSource() instanceof SharedSuggestionProvider isuggestionprovider) {
			//noinspection unchecked
			return isuggestionprovider.customSuggestion((CommandContext<SharedSuggestionProvider>) context, builder);
		}
		return Suggestions.empty();
	}

	public static BackpackUUIDArgumentType backpackUuid() {
		return new BackpackUUIDArgumentType();
	}
}
